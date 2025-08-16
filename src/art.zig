const std = @import("std");
const TaggedPointer = @import("tagged_pointer.zig").TaggedPointer;

const END_OF_KEY = 0;

pub fn AdaptiveRadixTree(comptime T: type) type {
    return struct {
        const Self = @This();

        const InnerNodeHeader = struct {
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
        };

        const NodeRef = struct {
            const TaggedNodePointer = TaggedPointer(union(enum) {
                node4: *Node4,
                node16: *Node16,
                node48: *Node48,
                node256: *Node256,
                leaf: *NodeLeaf,
            });

            ptr: TaggedNodePointer,

            fn init(value: TaggedNodePointer.Union) NodeRef {
                return .{ .ptr = TaggedNodePointer.init(value) };
            }

            fn getHeader(self: NodeRef) *InnerNodeHeader {
                return switch (self.ptr.get()) {
                    .node4 => |node4| &node4.header,
                    .node16 => |node16| &node16.header,
                    .node48 => |node48| &node48.header,
                    .node256 => |node256| &node256.header,
                    .leaf => @panic("getHeader should not be called on a leaf node"),
                };
            }

            /// Returns the compressed key path stored inside this node.
            ///
            /// If the prefix is small enough, it can be stored inline in the node.
            ///
            /// Otherwise, only the length of the prefix is stored and the full prefix is determined
            /// by reading the full key of the first leaf node inside this subtree. The depth must
            /// be passed so we can strip the depth prefix from the leaf's key.
            fn getPrefix(self: NodeRef, depth: usize) []const u8 {
                const header = self.getHeader();

                if (header.prefix_len <= 8) return header.partial_prefix_raw[0..header.prefix_len];

                // The prefix could not fit inside the node, so instead we will find the full
                // key by grabbing the first child leaf node and indexing into it based on the
                // current depth and the prefix length stored in the header.
                //
                // We are gauranteed that every inner node in an ART will always have at least 2
                // child nodes, so we know that a leaf node is available at all times.

                const leaf = self.min();
                std.debug.assert(leaf.key.len >= depth + header.prefix_len);

                return leaf.key[depth .. depth + header.prefix_len];
            }

            /// Sets the node's prefix to perform path compression.
            ///
            /// Only the first 8 bytes of the prefix are stored in the node itself, larger prefixes
            /// are stored as a length within the inner node and are computed in `getPrefix` by
            /// getting the prefix from the first leaf's key.
            fn setPrefix(
                self: NodeRef,
                prefix: []const u8,
                comptime copy_forwards: bool,
            ) void {
                const header = self.getHeader();
                const copy_len = @min(prefix.len, 8);
                header.prefix_len = @intCast(prefix.len);
                if (copy_forwards) {
                    std.mem.copyForwards(u8, header.partial_prefix_raw[0..copy_len], prefix[0..copy_len]);
                } else {
                    @memcpy(header.partial_prefix_raw[0..copy_len], prefix[0..copy_len]);
                }
            }

            /// Gets the length of the prefix match between this inner node and a key at a specific depth.
            fn checkPrefix(self: NodeRef, key: []const u8, depth: u64) u64 {
                return commonPrefixLength(self.getPrefix(depth), key[depth..]);
            }

            /// Finds the child with the associated key byte.
            fn findChild(self: NodeRef, byte: u8) ?*?NodeRef {
                return switch (self.ptr.get()) {
                    .node4 => |node4| node4.findChild(byte),
                    .node16 => |node16| node16.findChild(byte),
                    .node48 => |node48| node48.findChild(byte),
                    .node256 => |node256| node256.findChild(byte),
                    .leaf => @panic("findChild should not be called on a leaf node"),
                };
            }

            /// Adds a child to the node. There must not already be a child in this
            /// node with the same key byte.
            fn addChild(self: NodeRef, byte: u8, child: NodeRef) void {
                return switch (self.ptr.get()) {
                    .node4 => |node4| node4.addChild(byte, child),
                    .node16 => |node16| node16.addChild(byte, child),
                    .node48 => |node48| node48.addChild(byte, child),
                    .node256 => |node256| node256.addChild(byte, child),
                    .leaf => @panic("addChild should not be called on a leaf node"),
                };
            }

            /// Returns true if the node is full and needs to be grow to add a new child.
            ///
            /// It is not valid to call this on a NodeLeaf or Node256. The insert algorithm
            /// should never need to call this in those cases.
            fn isFull(self: NodeRef) bool {
                return self.getHeader().num_children ==
                    switch (self.ptr.get()) {
                        .node4 => Node4.MAX_CHILDREN,
                        .node16 => Node16.MAX_CHILDREN,
                        .node48 => Node48.MAX_CHILDREN,
                        .node256 => @panic("isFull show not be called on a Node256"),
                        .leaf => @panic("isFull should not be called on a leaf node"),
                    };
            }

            /// Grows the node into a larger size, (ie Node4 -> Node16).
            ///
            /// This creates a brand new node, the caller must deinit the old node.
            ///
            /// It is not valid to grow a NodeLeaf or Node256.
            fn grow(self: NodeRef, gpa: std.mem.Allocator) !NodeRef {
                const new_node = switch (self.ptr.get()) {
                    .node4 => |node4| try node4.grow(gpa),
                    .node16 => |node16| try node16.grow(gpa),
                    .node48 => |node48| try node48.grow(gpa),
                    .node256 => @panic("grow should not be called on a Node256"),
                    .leaf => @panic("grow should not be called on a leaf node"),
                };
                // No need to copy forward, slices do not overlap.
                const new_header = new_node.getHeader();
                const old_header = self.getHeader();
                new_header.prefix_len = old_header.prefix_len;
                @memcpy(&new_header.partial_prefix_raw, &old_header.partial_prefix_raw);
                return new_node;
            }

            /// Returns the leaf node with the smallest key within this subtree.
            ///
            /// It is expected that every inner node will at least have 2 children.
            /// This property should always be preserved by ART implementation
            /// because path compression prevents inner nodes that only have a single child.
            fn min(self: NodeRef) *const NodeLeaf {
                return switch (self.ptr.get()) {
                    .node4 => |node4| node4.min(),
                    .node16 => |node16| node16.min(),
                    .node48 => |node48| node48.min(),
                    .node256 => |node256| node256.min(),
                    .leaf => |leaf| leaf,
                };
            }

            fn deinit(self: NodeRef, gpa: std.mem.Allocator) void {
                return switch (self.ptr.get()) {
                    .node4 => |node4| node4.deinit(gpa),
                    .node16 => |node16| node16.deinit(gpa),
                    .node48 => |node48| node48.deinit(gpa),
                    .node256 => |node256| node256.deinit(gpa),
                    .leaf => |leaf| leaf.deinit(gpa),
                };
            }

            fn print(
                self: *const NodeRef,
                writer: anytype,
                depth: u64,
                indent: u64,
            ) !void {
                if (self.ptr.get() != .leaf) {
                    try std.fmt.format(writer, "Prefix \"{s}\" {s}", .{
                        self.getPrefix(depth),
                        @tagName(self.ptr.get()),
                    });
                }
                switch (self.ptr.get()) {
                    .node4 => |node4| try node4.print(writer, depth, indent + 1),
                    .node16 => |node16| try node16.print(writer, depth, indent + 1),
                    .node48 => |node48| try node48.print(writer, depth, indent + 1),
                    .node256 => |node256| try node256.print(writer, depth, indent + 1),
                    .leaf => |leaf| try leaf.print(writer),
                }

                if (indent == 0) {
                    try std.fmt.format(writer, "\n", .{});
                }
            }
        };

        const Node4 = struct {
            const MAX_CHILDREN: u16 = 4;

            header: InnerNodeHeader = .{},
            key: [MAX_CHILDREN]u8 = [_]u8{0} ** MAX_CHILDREN,
            children: [MAX_CHILDREN]?NodeRef = [_]?NodeRef{null} ** MAX_CHILDREN,

            fn init(gpa: std.mem.Allocator) !*Node4 {
                const node = try gpa.create(Node4);
                node.* = .{};
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
                    try child.?.print(writer, depth + 1 + self.header.prefix_len, indent);
                }
            }

            fn findChild(self: *Node4, byte: u8) ?*?NodeRef {
                std.debug.assert(self.header.num_children <= MAX_CHILDREN);

                for (self.key[0..self.header.num_children], 0..) |key_byte, i| {
                    if (key_byte == byte) {
                        return &self.children[i];
                    }
                }
                return null;
            }

            fn addChild(self: *Node4, byte: u8, child: NodeRef) void {
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
                insertAt(?NodeRef, self.children[0..self.header.num_children], index, child);
            }

            fn grow(self: *Node4, gpa: std.mem.Allocator) !NodeRef {
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
                return NodeRef.init(.{ .node16 = new_node });
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

            header: InnerNodeHeader = .{},
            key: [MAX_CHILDREN]u8 = [_]u8{0} ** MAX_CHILDREN,
            children: [MAX_CHILDREN]?NodeRef = [_]?NodeRef{null} ** MAX_CHILDREN,

            fn init(gpa: std.mem.Allocator) !*Node16 {
                const node = try gpa.create(Node16);
                node.* = .{};
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
                    try child.?.print(writer, depth + 1 + self.header.prefix_len, indent);
                }
            }

            fn findChild(self: *Node16, byte: u8) ?*?NodeRef {
                std.debug.assert(self.header.num_children <= MAX_CHILDREN);

                // TODO: Support fallback when SIMD is not available.

                // Finds the child by using SIMD operations to find the index of the key byte, if set.
                const cmp = @as(@Vector(16, u8), @splat(byte)) == @as(@Vector(16, u8), self.key);
                const mask: u16 = @intCast((@as(u17, 1) << @intCast(self.header.num_children)) - 1);
                const bitfield = @as(u16, @bitCast(cmp)) & mask;

                if (bitfield == 0) {
                    return null;
                }

                const index = @ctz(bitfield);
                return &self.children[index];
            }

            fn addChild(self: *Node16, byte: u8, child: NodeRef) void {
                std.debug.assert(self.header.num_children < MAX_CHILDREN);

                // TODO: Support fallback when SIMD is not available.

                // Finds the child by using SIMD operations to find the index of the first key byte that
                // is greater than the byte to be inserted.
                const cmp = @as(@Vector(16, u8), @splat(byte)) < @as(@Vector(16, u8), self.key);
                const mask: u16 = @intCast((@as(u17, 1) << @intCast(self.header.num_children)) - 1);
                const bitfield = @as(u16, @bitCast(cmp)) & mask;

                // If no larger key byte was found, we will insert the new child at the end of the node.
                const index = if (bitfield != 0) @ctz(bitfield) else self.header.num_children;

                self.header.num_children += 1;
                insertAt(u8, self.key[0..self.header.num_children], index, byte);
                insertAt(?NodeRef, self.children[0..self.header.num_children], index, child);
            }

            fn grow(self: *Node16, gpa: std.mem.Allocator) !NodeRef {
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
                return NodeRef.init(.{ .node48 = new_node });
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

            header: InnerNodeHeader = .{},
            key: [256]u8 = [_]u8{EMPTY} ** 256,
            children: [MAX_CHILDREN]?NodeRef = [_]?NodeRef{null} ** MAX_CHILDREN,

            fn init(gpa: std.mem.Allocator) !*Node48 {
                const node = try gpa.create(Node48);
                node.* = .{};
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
                for (self.key, 0..) |index, key| {
                    if (index == EMPTY) continue;

                    const child = self.children[index];
                    try std.fmt.format(writer, "\n", .{});
                    try pad(writer, indent);
                    try std.fmt.format(writer, "\"{c}\": ", .{@as(u8, @intCast(key))});
                    try child.?.print(writer, depth + 1 + self.header.prefix_len, indent);
                }
            }

            fn findChild(self: *Node48, byte: u8) ?*?NodeRef {
                std.debug.assert(self.header.num_children <= MAX_CHILDREN);

                const index = self.key[byte];
                if (index == EMPTY) return null;
                return &self.children[index];
            }

            fn addChild(self: *Node48, byte: u8, child: NodeRef) void {
                std.debug.assert(self.header.num_children < MAX_CHILDREN);
                // The key must not already exist in the node.
                std.debug.assert(self.key[byte] == EMPTY);

                const index = self.header.num_children;
                self.header.num_children += 1;
                self.children[index] = child;
                self.key[byte] = index;
            }

            fn grow(self: *Node48, gpa: std.mem.Allocator) !NodeRef {
                std.debug.assert(self.header.num_children == MAX_CHILDREN);

                const new_node = try Node256.init(gpa);
                errdefer new_node.deinit(gpa);

                for (self.key, 0..) |index, key_byte| {
                    if (index == EMPTY) continue;
                    new_node.addChild(@as(u8, @intCast(key_byte)), self.children[index].?);
                }

                // Ownership of the children is transferred to the new node.
                self.children = [_]?NodeRef{null} ** MAX_CHILDREN;
                return NodeRef.init(.{ .node256 = new_node });
            }

            fn min(self: *const Node48) *const NodeLeaf {
                std.debug.assert(self.header.num_children > 0);

                for (self.key) |index| {
                    if (index == EMPTY) continue;

                    const child = self.children[index];
                    std.debug.assert(child != null);
                    return child.?.min();
                }

                unreachable;
            }
        };

        const Node256 = struct {
            const MAX_CHILDREN: u16 = 256;

            header: InnerNodeHeader = .{},
            children: [MAX_CHILDREN]?NodeRef = [_]?NodeRef{null} ** MAX_CHILDREN,

            fn init(gpa: std.mem.Allocator) !*Node256 {
                const node = try gpa.create(Node256);
                node.* = .{};
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
                for (self.children, 0..) |child, key| {
                    if (child == null) continue;

                    try std.fmt.format(writer, "\n", .{});
                    try pad(writer, indent);
                    try std.fmt.format(writer, "\"{c}\": ", .{@as(u8, @intCast(key))});
                    try child.?.print(writer, depth + 1 + self.header.prefix_len, indent);
                }
            }

            fn findChild(self: *Node256, byte: u8) ?*?NodeRef {
                std.debug.assert(self.header.num_children <= MAX_CHILDREN);

                return &self.children[byte];
            }

            fn addChild(self: *Node256, byte: u8, child: NodeRef) void {
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
            key: []const u8,
            value: T,

            fn init(gpa: std.mem.Allocator, key: []const u8, value: T) !*NodeLeaf {
                const key_clone = try gpa.dupe(u8, key);
                errdefer gpa.free(key_clone);

                const node = try gpa.create(NodeLeaf);
                node.* = .{
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
        root: ?NodeRef = null,

        pub fn init(gpa: std.mem.Allocator) Self {
            return .{ .gpa = gpa };
        }

        pub fn deinit(self: *Self) void {
            if (self.root) |*root| {
                root.deinit(self.gpa);
            }
        }

        pub fn insert(self: *Self, key: []const u8, value: T) !?T {
            try validateKey(key);

            return try self.insertInner(&self.root, key, value, 0);
        }

        pub fn get(self: *Self, key: []const u8) ?T {
            validateKey(key) catch return null;

            if (search(self.root, key, 0)) |leaf| {
                return leaf.value;
            }

            return null;
        }

        fn validateKey(key: []const u8) !void {
            if (key.len == 0) {
                return error.EmptyKey;
            }
            if (std.mem.containsAtLeastScalar(u8, key, 1, END_OF_KEY)) {
                return error.InvalidKey;
            }
        }

        fn search(maybe_node: ?NodeRef, key: []const u8, depth: u64) ?*NodeLeaf {
            const node = maybe_node orelse return null;
            if (node.ptr.get() == .leaf) {
                const nodeLeaf = node.ptr.get().leaf;
                if (std.mem.eql(u8, nodeLeaf.key, key)) {
                    return nodeLeaf;
                }
                return null;
            }
            if (node.checkPrefix(key, depth) != node.getHeader().prefix_len) {
                return null;
            }
            const new_depth = depth + node.getHeader().prefix_len;
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
            maybe_node: *?NodeRef,
            key: []const u8,
            value: T,
            depth: u64,
        ) !?T {
            // If the node pointer is currently unset, that means we should insert the leaf in the slot.
            const node = maybe_node.* orelse {
                const leaf = try NodeLeaf.init(self.gpa, key, value);
                maybe_node.* = NodeRef.init(.{ .leaf = leaf });
                return null;
            };

            if (node.ptr.get() == .leaf) {
                const nodeLeaf = node.ptr.get().leaf;
                // If the keys of the two leaves match, the old leaf will be deleted and replaced by the new node.
                // TODO: As an optimization we could defer the allocation of the new leaf so we could
                // update the existing leaf in-place instead.
                if (std.mem.eql(u8, nodeLeaf.key, key)) {
                    const old_value = nodeLeaf.value;
                    nodeLeaf.value = value;
                    return old_value;
                }

                const leaf = try NodeLeaf.init(self.gpa, key, value);
                errdefer leaf.deinit(self.gpa);

                maybe_node.* = try splitLeaf(
                    self.gpa,
                    nodeLeaf,
                    leaf,
                    depth,
                );
                return null;
            }

            // Since we store prefixes in the inner nodes, we need to check to see
            // if this node requires the inner node to be split.

            const node_header = node.getHeader();
            const match_len = node.checkPrefix(key, depth);
            if (match_len != node.getHeader().prefix_len) {
                const leaf = try NodeLeaf.init(self.gpa, key, value);
                errdefer leaf.deinit(self.gpa);

                maybe_node.* = try splitInner(
                    self.gpa,
                    node,
                    leaf,
                    depth,
                    match_len,
                );
                return null;
            }

            // Continue traversing down the tree, if we find another child node, we will
            // recurse back into this function until we hit a leaf or a hole.
            const new_depth = depth + node_header.prefix_len;
            if (node.findChild(charAt(key, new_depth))) |next| {
                // We add 1 to the depth to account for the key byte stored in the inner node.
                return try self.insertInner(next, key, value, new_depth + 1);
            }

            // At this point there are no more child nodes, this means the key didn't
            // already exist in the tree and we are now ready to insert the node directly
            // into the tree.
            //
            const leaf = try NodeLeaf.init(self.gpa, key, value);
            errdefer leaf.deinit(self.gpa);

            // If the current node is full, we will grow the node by allocating a
            // new node, copying the children over, and deallocating the old node.

            if (node.isFull()) {
                const new_node = try node.grow(self.gpa);
                errdefer new_node.deinit(self.gpa);
                new_node.addChild(charAt(key, new_depth), NodeRef.init(.{ .leaf = leaf }));
                node.deinit(self.gpa);
                maybe_node.* = new_node;
                return null;
            }

            node.addChild(charAt(key, new_depth), NodeRef.init(.{ .leaf = leaf }));
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
        /// Insert "ABF" common prefix 2:
        ///
        ///       AB
        ///      /  \
        ///     CD   F
        ///    /  \
        ///   E    F
        ///
        fn splitInner(
            gpa: std.mem.Allocator,
            node: NodeRef,
            leaf: *NodeLeaf,
            depth: u64,
            common_prefix_len: u64,
        ) !NodeRef {
            const new_node = NodeRef.init(.{ .node4 = try Node4.init(gpa) });
            errdefer new_node.deinit(gpa);

            var node_partial_key = node.getPrefix(depth);
            const leaf_key = leaf.key;

            // The depth must be increased because we need to find the first character
            // of the key in each node after the common prefix that is different between
            // the nodes.
            const new_depth = depth + common_prefix_len;

            // Before mucking with the prefix keys, add the nodes to the new parent
            // using the first differentiating character in each's key.
            new_node.addChild(charAt(leaf_key, new_depth), NodeRef.init(.{ .leaf = leaf }));
            new_node.addChild(charAt(node_partial_key, common_prefix_len), node);

            // Update the new node to have the common prefix of the two nodes.
            // No need to copy forward as the slices do not overlap.
            new_node.setPrefix(node_partial_key[0..common_prefix_len], false);

            // We add one to the common prefix since 1 byte is used as a key
            // byte to distinguish the two children.
            // We must copy forward here since the slices may overlap.
            node.setPrefix(node.getPrefix(depth)[common_prefix_len + 1 ..], true);

            return new_node;
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
        ) !NodeRef {
            const new_node = NodeRef.init(.{ .node4 = try Node4.init(gpa) });
            errdefer new_node.deinit(gpa);

            const leaf_a_key = leaf_a.key;
            const leaf_b_key = leaf_b.key;

            // Find the common prefix of the nodes after the current depth. Since we only store
            // a partial prefix in each node, the max len of the prefix we can store is 8.
            const prefix_len = commonPrefixLength(leaf_a_key[depth..], leaf_b_key[depth..]);

            // No need to copy forwards here, the slices do not overlap.
            new_node.setPrefix(leaf_a_key[depth .. depth + prefix_len], false);

            // The depth must be increased because we need to find the first character
            // of the key in each leaf after the common prefix that is different between
            // the leaves.
            const new_depth = depth + prefix_len;

            new_node.addChild(charAt(leaf_a_key, new_depth), NodeRef.init(.{ .leaf = leaf_a }));
            new_node.addChild(charAt(leaf_b_key, new_depth), NodeRef.init(.{ .leaf = leaf_b }));

            return new_node;
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
    }

    while (pos < min_len and a[pos] == b[pos]) {
        pos += 1;
    }

    return pos;
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
    try testing.expectEqualStrings("", inner_a.getPrefix(inner_b.getHeader().prefix_len));
}

test "splitNode with large prefix" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const Tree = AdaptiveRadixTree(usize);

    const leaf_a = try Tree.NodeLeaf.init(gpa, "123456789_1/123456789_1", 1);
    errdefer leaf_a.deinit(gpa);

    const leaf_b = try Tree.NodeLeaf.init(gpa, "123456789_1/123456789_2", 1);
    errdefer leaf_b.deinit(gpa);

    const inner_a = try Tree.splitLeaf(gpa, leaf_a, leaf_b, 0);
    errdefer inner_a.deinit(gpa);

    try testing.expectEqualStrings("123456789_1/123456789_", inner_a.getPrefix(0));

    const leaf_c = try Tree.NodeLeaf.init(gpa, "123456789_2/123456789_1", 1);
    errdefer leaf_c.deinit(gpa);

    const common_prefix_len = inner_a.checkPrefix(leaf_c.key, 0);
    const inner_b = try Tree.splitInner(gpa, inner_a, leaf_c, 0, common_prefix_len);
    defer inner_b.deinit(gpa);

    try testing.expectEqualStrings("123456789_", inner_b.getPrefix(0));
    try testing.expectEqualStrings(
        "/123456789_",
        inner_a.getPrefix(inner_b.getHeader().prefix_len + 1),
    );
}

test "fuzz AdapativeRadixTree" {
    const Context = struct {
        fn testOne(context: @This(), input: []const u8) !void {
            _ = context;

            var gpa_impl = std.heap.GeneralPurposeAllocator(.{}).init;
            defer if (gpa_impl.deinit() == .leak) @panic("leaked memory in fuzz test");
            const gpa = gpa_impl.allocator();

            var arena_impl = std.heap.ArenaAllocator.init(gpa);
            defer arena_impl.deinit();
            const arena = arena_impl.allocator();

            var tree = AdaptiveRadixTree(usize).init(gpa);
            defer tree.deinit();
            var ref = std.StringHashMap(usize).init(arena);

            var parser = InputParser.init(input);
            while (parser.hasMore()) {
                const op = parser.readOperation() orelse break;

                switch (op) {
                    .insert => |data| {
                        _ = tree.insert(data.key, data.value) catch continue;
                        _ = ref.put(arena.dupe(u8, data.key) catch @panic("oom"), data.value) catch @panic("oom");
                    },
                    .get => |data| {
                        const actual = tree.get(data.key);
                        const expected = ref.get(data.key);
                        if (actual != expected) {
                            std.debug.print("Mismatch for key: {s}, expected: {any}, got: {any}\n", .{
                                data.key,
                                expected,
                                actual,
                            });
                            std.debug.print("Tree: {}\n", .{tree});
                            @panic("mismatch in get operation");
                        }
                    },
                }
            }
        }

        const Operation = union(enum) {
            insert: struct { key: []const u8, value: u64 },
            get: struct { key: []const u8 },

            pub fn format(
                self: @This(),
                comptime fmt: []const u8,
                options: std.fmt.FormatOptions,
                writer: anytype,
            ) !void {
                _ = options;
                _ = fmt;

                switch (self) {
                    .insert => |data| {
                        try writer.print("\x1b[32mINSERT\x1b[0m \x1b[36m\"{s}\"\x1b[0m \x1b[33m= {}\x1b[0m", .{ data.key, data.value });
                    },
                    .get => |data| {
                        try writer.print("\x1b[34mGET\x1b[0m \x1b[36m\"{s}\"\x1b[0m", .{data.key});
                    },
                }
            }
        };

        const InputParser = struct {
            data: []const u8,
            pos: usize,
            key_buffer: [256]u8,

            const SAFE_ASCII = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()-_=+[]{}|;:',.<>?/~`";

            fn init(data: []const u8) InputParser {
                return InputParser{ .data = data, .pos = 0, .key_buffer = [_]u8{0} ** 256 };
            }

            fn hasMore(self: *InputParser) bool {
                return self.pos < self.data.len;
            }

            fn readByte(self: *InputParser) ?u8 {
                if (self.pos >= self.data.len) return null;
                const byte = self.data[self.pos];
                self.pos += 1;
                return byte;
            }

            fn readBytes(self: *InputParser, len: usize) ?[]const u8 {
                if (self.pos + len > self.data.len) return null;
                const bytes = self.data[self.pos .. self.pos + len];
                self.pos += len;
                return bytes;
            }

            fn mapToSafeAscii(raw_bytes: []const u8, output_buffer: []u8) void {
                for (raw_bytes, 0..) |byte, i| {
                    output_buffer[i] = SAFE_ASCII[byte % SAFE_ASCII.len];
                }
            }

            fn readOperation(self: *InputParser) ?Operation {
                const type_byte = self.readByte() orelse return null;
                const is_insert = (type_byte % 2) == 0;

                // Read key length and ensure it's > 0
                const key_len_byte = self.readByte() orelse return null;
                const key_len = @max(1, @min(key_len_byte, self.key_buffer.len));

                // Read raw key bytes
                const raw_key = self.readBytes(key_len) orelse return null;

                // Map raw bytes to safe ASCII characters
                mapToSafeAscii(raw_key, self.key_buffer[0..key_len]);
                const key = self.key_buffer[0..key_len];

                if (is_insert) {
                    const value_bytes = self.readBytes(8) orelse return Operation{
                        .insert = .{ .key = key, .value = 0 },
                    };
                    const value = std.mem.readInt(u64, value_bytes[0..8], .little);

                    return Operation{
                        .insert = .{ .key = key, .value = value },
                    };
                } else {
                    return Operation{
                        .get = .{ .key = key },
                    };
                }
            }
        };
    };

    try std.testing.fuzz(Context{}, Context.testOne, .{});
}
