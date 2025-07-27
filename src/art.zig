const std = @import("std");

const END_OF_KEY = 0;

pub fn AdaptiveRadixTree(comptime T: type) type {
    return struct {
        const Self = @This();

        const NodeKind = enum { node4, node16, node48, node256, leaf };
        const NodeHeader = struct {
            kind: NodeKind,
            num_children: u8 = 0,
            prefix: [8]u8 = [_]u8{0} ** 8,
            prefix_len: u8 = 0,

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

            fn findChild(self: *NodeHeader, byte: u8) ?*?*NodeHeader {
                return switch (self.kind) {
                    .node4 => self.asNode4().findChild(byte),
                    .node16 => self.asNode16().findChild(byte),
                    .node48 => self.asNode48().findChild(byte),
                    .node256 => self.asNode256().findChild(byte),
                    .leaf => @panic("findChild should not be called on a leaf node"),
                };
            }

            fn addChild(self: *NodeHeader, byte: u8, child: *NodeHeader) void {
                return switch (self.kind) {
                    .node4 => self.asNode4().addChild(byte, child),
                    .node16 => self.asNode16().addChild(byte, child),
                    .node48 => self.asNode48().addChild(byte, child),
                    .node256 => self.asNode256().addChild(byte, child),
                    .leaf => @panic("addChild should not be called on a leaf node"),
                };
            }

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

            fn checkPrefix(self: *NodeHeader, key: []const u8, depth: u64) u8 {
                var i: u8 = 0;
                while (i < self.prefix_len and i + depth < key.len) : (i += 1) {
                    if (self.prefix[i] != key[depth + i]) {
                        break;
                    }
                }
                return i;
            }

            fn grow(self: *NodeHeader, gpa: std.mem.Allocator) !*NodeHeader {
                const new_node = switch (self.kind) {
                    .node4 => try self.asNode4().grow(gpa),
                    .node16 => try self.asNode16().grow(gpa),
                    .node48 => try self.asNode48().grow(gpa),
                    .node256 => @panic("grow should not be called on a Node256"),
                    .leaf => @panic("grow should not be called on a leaf node"),
                };
                new_node.prefix_len = self.prefix_len;
                @memcpy(new_node.prefix[0..self.prefix_len], self.prefix[0..self.prefix_len]);
                return new_node;
            }

            pub fn format(
                self: *const NodeHeader,
                comptime fmt: []const u8,
                options: std.fmt.FormatOptions,
                writer: anytype,
            ) !void {
                try std.fmt.format(
                    writer,
                    "Node {{ .prefix = \"{s}\", .node = ",
                    .{self.prefix[0..self.prefix_len]},
                );
                switch (self.kind) {
                    .node4 => try @constCast(self).asNode4().format(fmt, options, writer),
                    .node16 => try @constCast(self).asNode16().format(fmt, options, writer),
                    .node48 => try @constCast(self).asNode48().format(fmt, options, writer),
                    .node256 => try @constCast(self).asNode256().format(fmt, options, writer),
                    .leaf => try @constCast(self).asLeaf().format(fmt, options, writer),
                }
                try std.fmt.format(writer, " }}", .{});
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

            pub fn format(
                self: *const Node4,
                comptime fmt: []const u8,
                options: std.fmt.FormatOptions,
                writer: anytype,
            ) !void {
                _ = options;
                _ = fmt;
                try std.fmt.format(writer, "Node4 {{ ", .{});
                for (
                    self.key[0..self.header.num_children],
                    self.children[0..self.header.num_children],
                ) |key_byte, child| {
                    try std.fmt.format(writer, "{c} = {any}, ", .{ key_byte, child });
                }
                try std.fmt.format(writer, "}}", .{});
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

                // TODO: Binary search or SIMD?
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

            pub fn format(
                self: *const Node16,
                comptime fmt: []const u8,
                options: std.fmt.FormatOptions,
                writer: anytype,
            ) !void {
                _ = options;
                _ = fmt;
                try std.fmt.format(writer, "Node16 {{ ", .{});
                for (
                    self.key[0..self.header.num_children],
                    self.children[0..self.header.num_children],
                ) |key_byte, child| {
                    try std.fmt.format(writer, "{c} = {any}, ", .{ key_byte, child });
                }
                try std.fmt.format(writer, "}}", .{});
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

            pub fn format(
                self: *const Node48,
                comptime fmt: []const u8,
                options: std.fmt.FormatOptions,
                writer: anytype,
            ) !void {
                _ = options;
                _ = fmt;
                try std.fmt.format(writer, "Node48 {{ ", .{});
                for (self.key, 0..) |key_byte, i| {
                    if (key_byte == EMPTY) continue;
                    try std.fmt.format(writer, "{c} = {any}, ", .{ key_byte, self.children[i] });
                }
                try std.fmt.format(writer, "}}", .{});
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

            pub fn format(
                self: *const Node256,
                comptime fmt: []const u8,
                options: std.fmt.FormatOptions,
                writer: anytype,
            ) !void {
                _ = options;
                _ = fmt;
                try std.fmt.format(writer, "Node256 {{ ", .{});
                for (self.children, 0..) |child, key_byte| {
                    try std.fmt.format(writer, "{c} = {any}, ", .{
                        @as(u8, @intCast(key_byte)),
                        child,
                    });
                }
                try std.fmt.format(writer, "}}", .{});
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

            pub fn format(
                self: *const NodeLeaf,
                comptime fmt: []const u8,
                options: std.fmt.FormatOptions,
                writer: anytype,
            ) !void {
                _ = options;
                _ = fmt;
                try std.fmt.format(
                    writer,
                    "NodeLeaf {{ {s}={any} }}",
                    .{ self.key, self.value },
                );
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

        fn insertInner(
            self: *Self,
            maybe_node: *?*NodeHeader,
            key: []const u8,
            leaf: *NodeLeaf,
            depth: u64,
        ) !?T {
            const node = maybe_node.* orelse {
                maybe_node.* = &leaf.header;
                return null;
            };
            if (node.kind == .leaf) {
                if (std.mem.eql(u8, node.asLeaf().key, key)) {
                    const old_node = node;
                    const old_value = node.asLeaf().value;
                    maybe_node.* = &leaf.header;
                    old_node.deinit(self.gpa);
                    return old_value;
                }

                // Replace the leaf with a new node
                const new_node = try Node4.init(self.gpa);
                errdefer new_node.deinit(self.gpa);

                const key2 = node.asLeaf().key;

                var i: u8 = 0;
                while (i + depth < key.len and i + depth < key2.len and i < 8) : (i += 1) {
                    if (key[depth + i] != key2[depth + i]) break;
                    new_node.header.prefix[i] = key[depth + i];
                }
                new_node.header.prefix_len = i;

                const new_depth = depth + new_node.header.prefix_len;
                new_node.addChild(charAt(key, new_depth), &leaf.header);
                new_node.addChild(charAt(key2, new_depth), node);

                maybe_node.* = &new_node.header;
                return null;
            }
            const match_len = node.checkPrefix(key, depth);
            if (match_len != node.prefix_len) {
                var new_node = try Node4.init(self.gpa);
                errdefer new_node.deinit(self.gpa);

                new_node.addChild(charAt(key, depth + match_len), &leaf.header);
                new_node.addChild(charAt(&node.prefix, match_len), node);
                new_node.header.prefix_len = match_len;

                @memcpy(new_node.header.prefix[0..match_len], node.prefix[0..match_len]);
                std.mem.copyForwards(
                    u8,
                    &node.prefix,
                    node.prefix[match_len + 1 .. node.prefix_len],
                );
                node.prefix_len -= match_len + 1;

                maybe_node.* = &new_node.header;
                return null;
            }
            const new_depth = depth + node.prefix_len;
            if (node.findChild(charAt(key, new_depth))) |next| {
                return try self.insertInner(next, key, leaf, new_depth + 1);
            }

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
    for (0..200000) |i| {
        const str = try std.fmt.allocPrint(gpa, "key-{}", .{i});
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
    for (0..200000) |i| {
        const str = try std.fmt.allocPrint(gpa, "key-{}", .{i});
        defer gpa.free(str);
        _ = try a.insert(str, i);
        const val = a.get(str);
        try std.testing.expectEqual(i, val);
    }
}
