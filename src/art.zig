const std = @import("std");

const END_OF_KEY = 0;

pub fn AdaptiveRadixTree(comptime T: type) type {
    return struct {
        const Self = @This();

        const NodeKind = enum { node4, node16, node48, node256, leaf };
        const NodeHeader = struct {
            // TODO: Consider using tagged pointers, so we can move this tag to the
            // pointer instead.
            kind: NodeKind,
            // TODO: This space is taken up in leaf nodes too, might consider separating
            // out a header just for inner nodes?
            num_children: u8 = 0,

            // Partial prefix allows for optimistic path compression by merging multiple
            // single-child chains of nodes into a single node.
            //
            // The partial prefix has a max length of 8 to reduce size, once the prefix
            // exceeds 8, we switch to a pessimistic path compression approach where
            // we must validate the key during lookup by comparing it with the full key
            // in the leaf node.
            partial_prefix_raw: [8]u8 = [_]u8{0} ** 8,

            // The length of the compressed key path in this node. This may be greater than
            // the size of the partial prefix. In that case, the search algorithm must
            // validate the final key value stored in the leaf node.
            prefix_len: u64 = 0,

            fn init(gpa: std.mem.Allocator, kind: NodeKind) NodeHeader {
                const prefix_allocator = std.heap.stackFallback(8, gpa);
                return .{
                    .kind = kind,
                    .prefix_allocator = prefix_allocator,
                    .prefix = std.ArrayListUnmanaged(u8).initCapacity(prefix_allocator.get(), 8),
                };
            }

            /// Returns the compressed key path stored inside this node.
            ///
            /// If the prefix is small enough, it can be stored inline in the node.
            ///
            /// Otherwise, only the length of the prefix is stored and the full prefix is determined
            /// by reading the full key of the first leaf node inside this subtree. The depth must
            /// be passed so we can strip the depth prefix from the leaf's key.
            fn getPrefix(header: *const NodeHeader, depth: usize) []const u8 {
                if (header.prefix_len <= 8) return header.partial_prefix_raw[0..header.prefix_len];

                // The prefix could not fit inside the node, so instead we will find the full
                // key by grabbing the first child leaf node and indexing into it based on the
                // current depth and the prefix length stored in the header.
                //
                // We are gauranteed that every inner node in an ART will always have at least 2
                // child nodes, so we know that a leaf node is available at all times.

                const leaf = header.min();
                std.debug.assert(leaf.key.len >= depth + header.prefix_len);

                return leaf.key[depth .. depth + header.prefix_len];
            }

            /// Sets the node's prefix to perform path compression.
            ///
            /// Only the first 8 bytes of the prefix are stored in the node itself, larger prefixes
            /// are stored as a length within the inner node and are computed in `getPrefix` by
            /// getting the prefix from the first leaf's key.
            fn setPrefix(
                header: *NodeHeader,
                prefix: []const u8,
                comptime copy_forwards: bool,
            ) void {
                const copy_len = @min(prefix.len, 8);
                header.prefix_len = @intCast(prefix.len);
                if (copy_forwards) {
                    std.mem.copyForwards(u8, header.partial_prefix_raw[0..copy_len], prefix[0..copy_len]);
                } else {
                    @memcpy(header.partial_prefix_raw[0..copy_len], prefix[0..copy_len]);
                }
            }

            fn asNode4(header: *NodeHeader) *Node4 {
                std.debug.assert(header.kind == .node4);
                return @alignCast(@fieldParentPtr("header", header));
            }
            fn asNode16(header: *NodeHeader) *Node16 {
                std.debug.assert(header.kind == .node16);
                return @alignCast(@fieldParentPtr("header", header));
            }
            fn asNode48(header: *NodeHeader) *Node48 {
                std.debug.assert(header.kind == .node48);
                return @alignCast(@fieldParentPtr("header", header));
            }
            fn asNode256(header: *NodeHeader) *Node256 {
                std.debug.assert(header.kind == .node256);
                return @alignCast(@fieldParentPtr("header", header));
            }
            fn asLeaf(header: *NodeHeader) *NodeLeaf {
                std.debug.assert(header.kind == .leaf);
                return @alignCast(@fieldParentPtr("header", header));
            }

            fn asNode4Const(header: *const NodeHeader) *const Node4 {
                std.debug.assert(header.kind == .node4);
                return @alignCast(@fieldParentPtr("header", header));
            }
            fn asNode16Const(header: *const NodeHeader) *const Node16 {
                std.debug.assert(header.kind == .node16);
                return @alignCast(@fieldParentPtr("header", header));
            }
            fn asNode48Const(header: *const NodeHeader) *const Node48 {
                std.debug.assert(header.kind == .node48);
                return @alignCast(@fieldParentPtr("header", header));
            }
            fn asNode256Const(header: *const NodeHeader) *const Node256 {
                std.debug.assert(header.kind == .node256);
                return @alignCast(@fieldParentPtr("header", header));
            }
            fn asLeafConst(header: *const NodeHeader) *const NodeLeaf {
                std.debug.assert(header.kind == .leaf);
                return @alignCast(@fieldParentPtr("header", header));
            }

            /// Finds the child with the associated key byte.
            fn findChild(self: *NodeHeader, byte: u8) ?*?*NodeHeader {
                return switch (self.kind) {
                    .node4 => self.asNode4().findChild(byte),
                    .node16 => self.asNode16().findChild(byte),
                    .node48 => self.asNode48().findChild(byte),
                    .node256 => self.asNode256().findChild(byte),
                    .leaf => @panic("findChild should not be called on a leaf node"),
                };
            }

            /// Adds a child to the node. There must not already be a child in this
            /// node with the same key byte.
            fn addChild(self: *NodeHeader, byte: u8, child: *NodeHeader) void {
                return switch (self.kind) {
                    .node4 => self.asNode4().addChild(byte, child),
                    .node16 => self.asNode16().addChild(byte, child),
                    .node48 => self.asNode48().addChild(byte, child),
                    .node256 => self.asNode256().addChild(byte, child),
                    .leaf => @panic("addChild should not be called on a leaf node"),
                };
            }

            /// Returns true if the node is full and needs to be grow to add a new child.
            ///
            /// It is not valid to call this on a NodeLeaf or Node256. The insert algorithm
            /// should never need to call this in those cases.
            fn isFull(self: *NodeHeader) bool {
                return self.num_children ==
                    switch (self.kind) {
                        .node4 => Node4.MAX_CHILDREN,
                        .node16 => Node16.MAX_CHILDREN,
                        .node48 => Node48.MAX_CHILDREN,
                        .node256 => @panic("isFull show not be called on a Node256"),
                        .leaf => @panic("isFull should not be called on a leaf node"),
                    };
            }

            /// Gets the length of the prefix match between this inner node and a key at a specific depth.
            fn checkPrefix(self: *NodeHeader, key: []const u8, depth: u64) u64 {
                return commonPrefixLength(self.getPrefix(depth), key[depth..]);
            }

            /// Grows the node into a larger size, (ie Node4 -> Node16).
            ///
            /// This creates a brand new node, the caller must deinit the old node.
            ///
            /// It is not valid to grow a NodeLeaf or Node256.
            fn grow(self: *NodeHeader, gpa: std.mem.Allocator) !*NodeHeader {
                const new_node = switch (self.kind) {
                    .node4 => try self.asNode4().grow(gpa),
                    .node16 => try self.asNode16().grow(gpa),
                    .node48 => try self.asNode48().grow(gpa),
                    .node256 => @panic("grow should not be called on a Node256"),
                    .leaf => @panic("grow should not be called on a leaf node"),
                };
                // No need to copy forward, slices do not overlap.
                new_node.prefix_len = self.prefix_len;
                @memcpy(&new_node.partial_prefix_raw, &self.partial_prefix_raw);
                return new_node;
            }

            /// Returns the leaf node with the smallest key within this subtree.
            ///
            /// It is expected that every inner node will at least have 2 children.
            /// This property should always be preserved by ART implementation
            /// because path compression prevents inner nodes that only have a single child.
            fn min(self: *const NodeHeader) *const NodeLeaf {
                return switch (self.kind) {
                    .node4 => self.asNode4Const().min(),
                    .node16 => self.asNode16Const().min(),
                    .node48 => self.asNode48Const().min(),
                    .node256 => self.asNode256Const().min(),
                    .leaf => self.asLeafConst(),
                };
            }

            fn deinit(self: *NodeHeader, gpa: std.mem.Allocator) void {
                return switch (self.kind) {
                    .node4 => self.asNode4().deinit(gpa),
                    .node16 => self.asNode16().deinit(gpa),
                    .node48 => self.asNode48().deinit(gpa),
                    .node256 => self.asNode256().deinit(gpa),
                    .leaf => self.asLeaf().deinit(gpa),
                };
            }

            fn print(
                self: *const NodeHeader,
                writer: anytype,
                depth: u64,
                indent: u64,
            ) !void {
                if (self.kind != .leaf) {
                    try std.fmt.format(writer, "Prefix \"{s}\" {}", .{
                        self.getPrefix(depth),
                        self.kind,
                    });
                }
                switch (self.kind) {
                    .node4 => try self.asNode4Const().print(writer, depth, indent + 1),
                    .node16 => try self.asNode16Const().print(writer, depth, indent + 1),
                    .node48 => try self.asNode48Const().print(writer, depth, indent + 1),
                    .node256 => try self.asNode256Const().print(writer, depth, indent + 1),
                    .leaf => try self.asLeafConst().print(writer),
                }

                if (indent == 0) {
                    try std.fmt.format(writer, "\n", .{});
                }
            }
        };

        const Node4 = struct {
            const MAX_CHILDREN: u16 = 4;

            header: NodeHeader,
            key: [MAX_CHILDREN]u8,
            children: [MAX_CHILDREN]?*NodeHeader,

            fn init(gpa: std.mem.Allocator) !*Node4 {
                const node = try gpa.create(Node4);
                node.* = .{
                    .header = .{ .kind = .node4 },
                    .key = [_]u8{0} ** MAX_CHILDREN,
                    .children = [_]?*NodeHeader{null} ** MAX_CHILDREN,
                };
                return node;
            }

            fn deinit(self: *Node4, gpa: std.mem.Allocator) void {
                for (self.children[0..self.header.num_children]) |child| {
                    if (child) |c| {
                        c.deinit(gpa);
                    }
                }
                gpa.destroy(self);
            }

            fn print(
                self: *const Node4,
                writer: anytype,
                depth: u64,
                indent: u64,
            ) anyerror!void {
                for (
                    self.key[0..self.header.num_children],
                    self.children[0..self.header.num_children],
                ) |key, child| {
                    try std.fmt.format(writer, "\n", .{});
                    try pad(writer, indent);
                    try std.fmt.format(writer, "\"{c}\": ", .{key});
                    try child.?.print(writer, depth + self.header.prefix_len, indent);
                }
            }

            fn findChild(self: *Node4, byte: u8) ?*?*NodeHeader {
                std.debug.assert(self.header.num_children <= MAX_CHILDREN);

                for (self.key, 0..) |key_byte, i| {
                    if (key_byte == byte) {
                        return &self.children[i];
                    }
                }
                return null;
            }

            fn addChild(self: *Node4, byte: u8, child: *NodeHeader) void {
                std.debug.assert(self.header.num_children < MAX_CHILDREN);

                var index: u8 = 0;
                while (index < self.header.num_children) : (index += 1) {
                    if (self.key[index] == byte) {
                        @panic("Key already exists in the node");
                    }
                    if (self.key[index] > byte) {
                        break;
                    }
                }

                self.header.num_children += 1;
                insertAt(u8, self.key[0..self.header.num_children], index, byte);
                insertAt(?*NodeHeader, self.children[0..self.header.num_children], index, child);
            }

            fn grow(self: *Node4, gpa: std.mem.Allocator) !*NodeHeader {
                std.debug.assert(self.header.num_children == MAX_CHILDREN);

                const new_node = try Node16.init(gpa);
                errdefer new_node.deinit(gpa);

                for (
                    self.key[0..self.header.num_children],
                    self.children[0..self.header.num_children],
                ) |key_byte, child| {
                    new_node.addChild(key_byte, child.?);
                }
                // Ownership of the children is transferred to the new node.
                self.header.num_children = 0;
                return &new_node.header;
            }

            fn min(self: *const Node4) *const NodeLeaf {
                std.debug.assert(self.header.num_children > 0);

                const index = std.mem.indexOfMin(u8, self.key[0..self.header.num_children]);
                const child = self.children[index];
                std.debug.assert(child != null);
                return child.?.min();
            }
        };

        const Node16 = struct {
            const MAX_CHILDREN: u16 = 16;

            header: NodeHeader,
            key: [MAX_CHILDREN]u8,
            children: [MAX_CHILDREN]?*NodeHeader,

            fn init(gpa: std.mem.Allocator) !*Node16 {
                const node = try gpa.create(Node16);
                node.* = .{
                    .header = .{ .kind = .node16 },
                    .key = [_]u8{0} ** MAX_CHILDREN,
                    .children = [_]?*NodeHeader{null} ** MAX_CHILDREN,
                };
                return node;
            }

            fn deinit(self: *Node16, gpa: std.mem.Allocator) void {
                for (self.children[0..self.header.num_children]) |child| {
                    if (child) |c| {
                        c.deinit(gpa);
                    }
                }
                gpa.destroy(self);
            }

            fn print(
                self: *const Node16,
                writer: anytype,
                depth: u64,
                indent: u64,
            ) anyerror!void {
                for (
                    self.key[0..self.header.num_children],
                    self.children[0..self.header.num_children],
                ) |key, child| {
                    try std.fmt.format(writer, "\n", .{});
                    try pad(writer, indent);
                    try std.fmt.format(writer, "\"{c}\": ", .{key});
                    try child.?.print(writer, depth + self.header.prefix_len, indent);
                }
            }

            fn findChild(self: *Node16, byte: u8) ?*?*NodeHeader {
                std.debug.assert(self.header.num_children <= MAX_CHILDREN);

                // Perform a SIMD comparison of the provided byte against the keys.
                // This allows for a highly efficient search.
                const key: @Vector(16, u8) = @splat(byte);
                const node_key: @Vector(16, u8) = self.key;
                const cmp = key == node_key;
                const mask: u16 = (@as(u16, 1) << @intCast(self.header.num_children)) - 1;
                const bitfield = @as(u16, @bitCast(cmp)) & mask;

                if (bitfield == 0) {
                    return null;
                }

                const index = @ctz(bitfield);
                return &self.children[index];
            }

            fn addChild(self: *Node16, byte: u8, child: *NodeHeader) void {
                std.debug.assert(self.header.num_children < MAX_CHILDREN);

                // TODO: We may not need to preserve sort order since we do fast SIMD lookups.
                // This could improve insertion speed.
                // We could also use SIMD to figure out the insertion point quicker.

                var index: u8 = 0;
                while (index < self.header.num_children) : (index += 1) {
                    if (self.key[index] == byte) {
                        @panic("Key already exists in the node");
                    }
                    if (self.key[index] > byte) {
                        break;
                    }
                }

                self.header.num_children += 1;
                insertAt(u8, self.key[0..self.header.num_children], index, byte);
                insertAt(?*NodeHeader, self.children[0..self.header.num_children], index, child);
            }

            fn grow(self: *Node16, gpa: std.mem.Allocator) !*NodeHeader {
                std.debug.assert(self.header.num_children == MAX_CHILDREN);

                const new_node = try Node48.init(gpa);
                errdefer new_node.deinit(gpa);

                for (
                    self.key[0..self.header.num_children],
                    self.children[0..self.header.num_children],
                ) |key_byte, child| {
                    new_node.addChild(key_byte, child.?);
                }
                // Ownership of the children is transferred to the new node.
                self.header.num_children = 0;
                return &new_node.header;
            }

            fn min(self: *const Node16) *const NodeLeaf {
                std.debug.assert(self.header.num_children > 0);

                // TODO: Can probably use SIMD to speed up.
                const index = std.mem.indexOfMin(u8, self.key[0..self.header.num_children]);
                const child = self.children[index];
                std.debug.assert(child != null);
                return child.?.min();
            }
        };

        const Node48 = struct {
            const MAX_CHILDREN: u16 = 48;

            const EMPTY = std.math.maxInt(u8);

            header: NodeHeader,
            key: [256]u8 = [_]u8{EMPTY} ** 256,
            children: [MAX_CHILDREN]?*NodeHeader,

            fn init(gpa: std.mem.Allocator) !*Node48 {
                const node = try gpa.create(Node48);
                node.* = .{
                    .header = .{ .kind = .node48 },
                    .key = [_]u8{0} ** 256,
                    .children = [_]?*NodeHeader{null} ** MAX_CHILDREN,
                };
                return node;
            }

            fn deinit(self: *Node48, gpa: std.mem.Allocator) void {
                for (self.children[0..self.header.num_children]) |child| {
                    if (child) |c| {
                        c.deinit(gpa);
                    }
                }
                gpa.destroy(self);
            }

            fn print(
                self: *const Node48,
                writer: anytype,
                depth: u64,
                indent: u64,
            ) anyerror!void {
                for (
                    self.key[0..self.header.num_children],
                    0..,
                ) |index, key| {
                    if (index == EMPTY) continue;

                    const child = self.children[index];
                    try std.fmt.format(writer, "\n", .{});
                    try pad(writer, indent);
                    try std.fmt.format(writer, "\"{c}\": ", .{@as(u8, @intCast(key))});
                    try child.?.print(writer, depth + self.header.prefix_len, indent);
                }
            }

            fn findChild(self: *Node48, byte: u8) ?*?*NodeHeader {
                std.debug.assert(self.header.num_children <= MAX_CHILDREN);

                const index = self.key[byte];
                if (index == EMPTY) return null;
                return &self.children[index];
            }

            fn addChild(self: *Node48, byte: u8, child: *NodeHeader) void {
                std.debug.assert(self.header.num_children < MAX_CHILDREN);
                // The key must not already exist in the node.
                std.debug.assert(self.key[byte] == EMPTY);

                const index = self.header.num_children;
                self.header.num_children += 1;
                self.children[index] = child;
                self.key[byte] = index;
            }

            fn grow(self: *Node48, gpa: std.mem.Allocator) !*NodeHeader {
                std.debug.assert(self.header.num_children == MAX_CHILDREN);

                const new_node = try Node256.init(gpa);
                errdefer new_node.deinit(gpa);

                for (self.key, 0..) |key_byte, i| {
                    if (key_byte == EMPTY) continue;
                    new_node.addChild(key_byte, self.children[i].?);
                }

                // Ownership of the children is transferred to the new node.
                self.children = [_]?*NodeHeader{null} ** MAX_CHILDREN;
                return &new_node.header;
            }

            fn min(self: *const Node48) *const NodeLeaf {
                std.debug.assert(self.header.num_children > 0);

                for (self.key, 0..) |key, index| {
                    if (key != EMPTY) {
                        const child = self.children[index];
                        std.debug.assert(child != null);
                        return child.?.min();
                    }
                }

                unreachable;
            }
        };

        const Node256 = struct {
            const MAX_CHILDREN: u16 = 256;

            header: NodeHeader,
            children: [MAX_CHILDREN]?*NodeHeader,

            fn init(gpa: std.mem.Allocator) !*Node256 {
                const node = try gpa.create(Node256);
                node.* = .{
                    .header = .{ .kind = .node256, .num_children = 0 },
                    .children = [_]?*NodeHeader{null} ** MAX_CHILDREN,
                };
                return node;
            }

            fn deinit(self: *Node256, gpa: std.mem.Allocator) void {
                for (self.children) |child| {
                    if (child) |c| {
                        c.deinit(gpa);
                    }
                }
                gpa.destroy(self);
            }

            fn print(
                self: *const Node256,
                writer: anytype,
                depth: u64,
                indent: u64,
            ) anyerror!void {
                for (
                    self.children[0..self.header.num_children],
                    0..,
                ) |child, key| {
                    if (child == null) continue;

                    try std.fmt.format(writer, "\n", .{});
                    try pad(writer, indent);
                    try std.fmt.format(writer, "\"{c}\": ", .{@as(u8, @intCast(key))});
                    try child.?.print(writer, depth + self.header.prefix_len, indent);
                }
            }

            fn findChild(self: *Node256, byte: u8) ?*?*NodeHeader {
                std.debug.assert(self.header.num_children <= MAX_CHILDREN);

                return &self.children[byte];
            }

            fn addChild(self: *Node256, byte: u8, child: *NodeHeader) void {
                std.debug.assert(self.header.num_children < MAX_CHILDREN);
                // The key must not already exist in the node.
                std.debug.assert(self.children[byte] == null);

                self.header.num_children += 1;
                self.children[byte] = child;
            }

            fn min(self: *const Node256) *const NodeLeaf {
                std.debug.assert(self.header.num_children > 0);

                for (self.children) |child| {
                    if (child != null) return child.?.min();
                }

                unreachable;
            }
        };

        const NodeLeaf = struct {
            header: NodeHeader,
            key: []const u8,
            value: T,

            fn init(gpa: std.mem.Allocator, key: []const u8, value: T) !*NodeLeaf {
                const key_clone = try gpa.dupe(u8, key);
                errdefer gpa.free(key_clone);

                const node = try gpa.create(NodeLeaf);
                node.* = .{
                    .header = .{ .kind = .leaf },
                    .key = key_clone,
                    .value = value,
                };
                return node;
            }

            fn deinit(self: *NodeLeaf, gpa: std.mem.Allocator) void {
                gpa.free(self.key);
                gpa.destroy(self);
            }

            fn print(self: *const NodeLeaf, writer: anytype) anyerror!void {
                try std.fmt.format(writer, "\"{s}\" = {}", .{ self.key, self.value });
            }
        };

        gpa: std.mem.Allocator,
        root: ?*NodeHeader = null,

        pub fn init(gpa: std.mem.Allocator) Self {
            return .{ .gpa = gpa };
        }

        pub fn deinit(self: *Self) void {
            if (self.root) |root| {
                root.deinit(self.gpa);
            }
        }

        pub fn insert(self: *Self, key: []const u8, value: T) !?T {
            const leaf = try NodeLeaf.init(self.gpa, key, value);
            errdefer leaf.deinit(self.gpa);

            return try self.insertInner(&self.root, key, leaf, 0);
        }

        pub fn get(self: *Self, key: []const u8) ?T {
            if (search(self.root, key, 0)) |leaf| {
                return leaf.value;
            }
            return null;
        }

        fn search(maybe_node: ?*NodeHeader, key: []const u8, depth: u64) ?*NodeLeaf {
            const node = maybe_node orelse return null;
            if (node.kind == .leaf) {
                if (std.mem.eql(u8, node.asLeaf().key, key)) {
                    return node.asLeaf();
                }
                return null;
            }
            if (node.checkPrefix(key, depth) != node.prefix_len) {
                return null;
            }
            const new_depth = depth + node.prefix_len;
            const child = node.findChild(charAt(key, new_depth)) orelse return null;
            return search(child.*, key, new_depth + 1);
        }

        pub fn format(
            self: *const Self,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;
            if (self.root) |root| {
                try root.print(writer, 0, 0);
            } else {
                try std.fmt.format(writer, "<empty tree>", .{});
            }
        }

        /// Inserts a node in the tree by recursively descending down the tree until an insertion
        /// point is found. `maybe_node` is a pointer to a "slot" where a node can be inserted.
        ///
        /// When `maybe_node` is replace and it was non-null, the old node is freed.
        fn insertInner(
            self: *Self,
            maybe_node: *?*NodeHeader,
            key: []const u8,
            leaf: *NodeLeaf,
            depth: u64,
        ) !?T {
            // If the node pointer is currently unset, that means we should insert the leaf in the slot.
            const node = maybe_node.* orelse {
                maybe_node.* = &leaf.header;
                return null;
            };

            if (node.kind == .leaf) {
                // If the keys of the two leaves match, the old leaf will be deleted and replaced by the new node.
                // TODO: As an optimization we could defer the allocation of the new leaf so we could
                // update the existing leaf in-place instead.
                if (std.mem.eql(u8, node.asLeaf().key, key)) {
                    const old_node = node;
                    const old_value = node.asLeaf().value;
                    maybe_node.* = &leaf.header;
                    old_node.deinit(self.gpa);
                    return old_value;
                }

                maybe_node.* = try splitLeaf(self.gpa, node.asLeaf(), leaf, depth);
                return null;
            }

            // Since we store prefixes in the inner nodes, we need to check to see
            // if this node requires the inner node to be split.

            const match_len = node.checkPrefix(key, depth);
            if (match_len != node.prefix_len) {
                maybe_node.* = try splitInner(self.gpa, node, leaf, depth, match_len);
                return null;
            }

            // Continue traversing down the tree, if we find another child node, we will
            // recurse back into this function until we hit a leaf or a hole.
            const new_depth = depth + node.prefix_len;
            if (node.findChild(charAt(key, new_depth))) |next| {
                return try self.insertInner(next, key, leaf, new_depth + 1);
            }

            // At this point there are no more child nodes, this means the key didn't
            // already exist in the tree and we are now ready to insert the node directly
            // into the tree.

            // If the current node is full, we will grow the node by allocating a
            // new node, copying the children over, and deallocating the old node.
            if (node.isFull()) {
                const new_node = try node.grow(self.gpa);
                errdefer new_node.deinit(self.gpa);
                new_node.addChild(charAt(key, new_depth), &leaf.header);
                node.deinit(self.gpa);
                maybe_node.* = new_node;
                return null;
            }

            node.addChild(charAt(key, new_depth), &leaf.header);
            return null;
        }

        /// Given a non-leaf node and a leaf node that needs to be inserted where the leaf node
        /// does not share the full prefix of the non-leaf node, this will split the node by creating
        /// a new parent node and adding the non-leaf and leaf nodes as children, recording their
        /// common prefix.
        ///
        /// The common prefix length is passed in because it is calculated before this function
        /// is called.
        ///
        /// The new parent node will be returned and must be freed by the caller.
        ///
        /// Before:
        ///     ABCD
        ///     /  \
        ///    E    F
        ///
        /// Insert "ABF":
        ///
        ///       AB
        ///      /  \
        ///     CD   F
        ///    /  \
        ///   E    F
        ///
        fn splitInner(
            gpa: std.mem.Allocator,
            node: *NodeHeader,
            leaf: *NodeLeaf,
            depth: u64,
            common_prefix_len: u64,
        ) !*NodeHeader {
            const new_node = try Node4.init(gpa);
            errdefer new_node.deinit(gpa);

            const node_partial_key = node.getPrefix(depth);
            const leaf_key = leaf.key;

            // Update the new node to have the common prefix of the two nodes.
            // No need to copy forward as the slices do not overlap.
            new_node.header.setPrefix(node_partial_key[0..common_prefix_len], false);

            // We must copy forward here since the slices may overlap.
            node.setPrefix(node.getPrefix(depth)[common_prefix_len..], true);

            // The depth must be increased because we need to find the first character
            // of the key in each node after the common prefix that is different between
            // the nodes.
            const new_depth = depth + common_prefix_len;

            new_node.addChild(charAt(leaf_key, new_depth), &leaf.header);
            new_node.addChild(charAt(node_partial_key, common_prefix_len), node);

            return &new_node.header;
        }

        /// Given two leaf nodes and the depth in the tree, creates a new parent node with both leaves as children.
        /// The common prefix of the two leaf nodes after the current depth will be stored on the parent node.
        ///
        /// The new parent node will be returned and must be freed by the caller.
        ///
        /// Before:
        ///
        /// ABC
        ///
        /// Insert ABD:
        ///
        ///     AB
        ///    /  \
        ///   C    D
        ///
        fn splitLeaf(
            gpa: std.mem.Allocator,
            leaf_a: *NodeLeaf,
            leaf_b: *NodeLeaf,
            depth: u64,
        ) !*NodeHeader {
            const new_node = try Node4.init(gpa);
            errdefer new_node.deinit(gpa);

            const leaf_a_key = leaf_a.key;
            const leaf_b_key = leaf_b.key;

            // Find the common prefix of the nodes after the current depth. Since we only store
            // a partial prefix in each node, the max len of the prefix we can store is 8.
            const prefix_len = commonPrefixLength(leaf_a_key[depth..], leaf_b_key[depth..]);

            // No need to copy forwards here, the slices do not overlap.
            new_node.header.setPrefix(leaf_a_key[depth .. depth + prefix_len], false);

            // The depth must be increased because we need to find the first character
            // of the key in each leaf after the common prefix that is different between
            // the leaves.
            const new_depth = depth + prefix_len;

            new_node.addChild(charAt(leaf_a_key, new_depth), &leaf_a.header);
            new_node.addChild(charAt(leaf_b_key, new_depth), &leaf_b.header);

            return &new_node.header;
        }
    };
}

/// Returns the character at the given index in the slice.
/// If the index is equal to the length of the slice,
/// it returns an END_OF_KEY sentinel value.
fn charAt(slice: []const u8, index: usize) u8 {
    std.debug.assert(index <= slice.len);
    if (index == slice.len) {
        return END_OF_KEY;
    }
    return slice[index];
}

fn commonPrefix(a: []const u8, b: []const u8) []const u8 {
    return a[0..commonPrefixLength(a, b)];
}
fn commonPrefixLength(a: []const u8, b: []const u8) u64 {
    const min_len = @min(a.len, b.len);
    if (min_len == 0) {
        return 0;
    }

    var pos: u64 = 0;
    if (comptime std.simd.suggestVectorLength(u8)) |vector_len| {
        const chunk_size = @min(vector_len, 32);

        while (pos + chunk_size <= min_len) {
            const vec_a: @Vector(chunk_size, u8) = a[pos..][0..chunk_size].*;
            const vec_b: @Vector(chunk_size, u8) = b[pos..][0..chunk_size].*;

            const xor = vec_a ^ vec_b;
            const all_zero = @reduce(.Or, xor) == 0;

            if (all_zero) {
                pos += chunk_size;
            } else {
                break;
            }
        }

        while (pos < min_len and a[pos] == b[pos]) {
            pos += 1;
        }
        return pos;
    }
}

fn pad(writer: anytype, amount: u64) !void {
    for (0..amount) |_| try std.fmt.format(writer, "\t", .{});
}

/// Inserts an element at the specified index in the slice.
/// All elements after the index are shifted to the right.
/// The rightmost element is discarded if the slice is full.
fn insertAt(comptime T: type, slice: []T, index: usize, element: T) void {
    std.debug.assert(index <= slice.len);
    if (index < slice.len - 1) {
        std.mem.copyBackwards(T, slice[index + 1 ..], slice[index .. slice.len - 1]);
    }
    slice[index] = element;
}

test AdaptiveRadixTree {
    const gpa = std.testing.allocator;
    var a = AdaptiveRadixTree(usize).init(gpa);
    defer a.deinit();
    for (0..100) |i| {
        const str = try std.fmt.allocPrint(gpa, "keeeeeeeeeeeeeey-{}", .{i});
        defer gpa.free(str);
        _ = try a.insert(str, i);
    }
    _ = try a.insert("A", 10);
    _ = try a.insert("AB", 10);
    _ = try a.insert("AAB", 10);
    _ = try a.insert("AAAB", 10);
    _ = try a.insert("AAAAB", 10);
    _ = try a.insert("AB", 10);
    _ = try a.insert("B", 10);
    for (0..100) |i| {
        const str = try std.fmt.allocPrint(gpa, "keeeeeeeeeeeeeey-{}", .{i});
        defer gpa.free(str);
        _ = try a.insert(str, i);
        const val = a.get(str);
        try std.testing.expectEqual(i, val);
    }
}

test commonPrefix {
    const testing = std.testing;

    // Test cases
    try testing.expectEqual(commonPrefixLength("hello", "help"), 3);
    try testing.expectEqual(commonPrefixLength("abc", "xyz"), 0);
    try testing.expectEqual(commonPrefixLength("same", "same"), 4);
    try testing.expectEqual(commonPrefixLength("", "anything"), 0);
    try testing.expectEqual(commonPrefixLength("test", ""), 0);

    // Test with longer strings to exercise SIMD path
    const long_a = "this is a very long string with many characters to test SIMD processing";
    const long_b = "this is a very long string with many different characters here";
    try testing.expectEqual(commonPrefixLength(long_a, long_b), 37);
    try testing.expectEqual(commonPrefixLength("hello", "help"), 3);
    try testing.expectEqual(commonPrefixLength("abcdefgh12345678", "abcdefgh87654321"), 8);
}

test "mergeLeaves with small prefix" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const Tree = AdaptiveRadixTree(usize);

    const leaf_a = try Tree.NodeLeaf.init(gpa, "ABC", 1);
    errdefer leaf_a.deinit(gpa);

    const leaf_b = try Tree.NodeLeaf.init(gpa, "ABD", 1);
    errdefer leaf_b.deinit(gpa);

    const merged = try Tree.splitLeaf(gpa, leaf_a, leaf_b, 0);
    defer merged.deinit(gpa);

    try testing.expectEqualStrings("AB", merged.getPrefix(0));
}

test "mergeLeaves with large prefix" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const Tree = AdaptiveRadixTree(usize);

    const leaf_a = try Tree.NodeLeaf.init(gpa, "00000000001", 1);
    errdefer leaf_a.deinit(gpa);

    const leaf_b = try Tree.NodeLeaf.init(gpa, "00000000002", 1);
    errdefer leaf_b.deinit(gpa);

    const merged = try Tree.splitLeaf(gpa, leaf_a, leaf_b, 0);
    defer merged.deinit(gpa);

    try testing.expectEqualStrings("0000000000", merged.getPrefix(0));
}

test "splitNode with small prefix" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const Tree = AdaptiveRadixTree(usize);

    const leaf_a = try Tree.NodeLeaf.init(gpa, "ABC", 1);
    errdefer leaf_a.deinit(gpa);

    const leaf_b = try Tree.NodeLeaf.init(gpa, "ABD", 1);
    errdefer leaf_b.deinit(gpa);

    const inner_a = try Tree.splitLeaf(gpa, leaf_a, leaf_b, 0);
    errdefer inner_a.deinit(gpa);

    try testing.expectEqualStrings("AB", inner_a.getPrefix(0));

    const leaf_c = try Tree.NodeLeaf.init(gpa, "AE", 1);
    errdefer leaf_c.deinit(gpa);

    const common_prefix_len = inner_a.checkPrefix(leaf_c.key, 0);
    const inner_b = try Tree.splitInner(gpa, inner_a, leaf_c, 0, common_prefix_len);
    defer inner_b.deinit(gpa);

    try testing.expectEqualStrings("A", inner_b.getPrefix(0));
    try testing.expectEqualStrings("B", inner_a.getPrefix(inner_b.prefix_len));
}

test "splitNode with large prefix" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const Tree = AdaptiveRadixTree(usize);

    const leaf_a = try Tree.NodeLeaf.init(gpa, "0000000001/0000000001", 1);
    errdefer leaf_a.deinit(gpa);

    const leaf_b = try Tree.NodeLeaf.init(gpa, "0000000001/0000000002", 1);
    errdefer leaf_b.deinit(gpa);

    const inner_a = try Tree.splitLeaf(gpa, leaf_a, leaf_b, 0);
    errdefer inner_a.deinit(gpa);

    try testing.expectEqualStrings("0000000001/000000000", inner_a.getPrefix(0));

    const leaf_c = try Tree.NodeLeaf.init(gpa, "0000000002/0000000001", 1);
    errdefer leaf_c.deinit(gpa);

    const common_prefix_len = inner_a.checkPrefix(leaf_c.key, 0);
    const inner_b = try Tree.splitInner(gpa, inner_a, leaf_c, 0, common_prefix_len);
    defer inner_b.deinit(gpa);

    try testing.expectEqualStrings("000000000", inner_b.getPrefix(0));
    try testing.expectEqualStrings("1/000000000", inner_a.getPrefix(inner_b.prefix_len));
}
