const std = @import("std");
const NodeRef = @import("node.zig").NodeRef;
const NodeLeaf = @import("node_leaf.zig").NodeLeaf;
const Node4 = @import("node4.zig").Node4;
const util = @import("util.zig");
const commonPrefixLength = util.commonPrefixLength;
const charAt = util.charAt;
const END_OF_KEY = util.END_OF_KEY;

pub fn AdaptiveRadixTree(comptime T: type) type {
    return struct {
        const Self = @This();

        gpa: std.mem.Allocator,
        root: ?NodeRef(T) = null,

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

        fn search(maybe_node: ?NodeRef(T), key: []const u8, depth: u64) ?*NodeLeaf(T) {
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
            maybe_node: *?NodeRef(T),
            key: []const u8,
            value: T,
            depth: u64,
        ) !?T {
            // If the node pointer is currently unset, that means we should insert the leaf in the slot.
            const node = maybe_node.* orelse {
                const leaf = try NodeLeaf(T).init(self.gpa, key, value);
                maybe_node.* = NodeRef(T).init(.{ .leaf = leaf });
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

                const leaf = try NodeLeaf(T).init(self.gpa, key, value);
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
                const leaf = try NodeLeaf(T).init(self.gpa, key, value);
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
            const leaf = try NodeLeaf(T).init(self.gpa, key, value);
            errdefer leaf.deinit(self.gpa);

            // If the current node is full, we will grow the node by allocating a
            // new node, copying the children over, and deallocating the old node.

            if (node.isFull()) {
                const new_node = try node.grow(self.gpa);
                errdefer new_node.deinit(self.gpa);
                new_node.addChild(charAt(key, new_depth), NodeRef(T).init(.{ .leaf = leaf }));
                node.deinit(self.gpa);
                maybe_node.* = new_node;
                return null;
            }

            node.addChild(charAt(key, new_depth), NodeRef(T).init(.{ .leaf = leaf }));
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
            node: NodeRef(T),
            leaf: *NodeLeaf(T),
            depth: u64,
            common_prefix_len: u64,
        ) !NodeRef(T) {
            const new_node = NodeRef(T).init(.{ .node4 = try Node4(T).init(gpa) });
            errdefer new_node.deinit(gpa);

            var node_partial_key = node.getPrefix(depth);
            const leaf_key = leaf.key;

            // The depth must be increased because we need to find the first character
            // of the key in each node after the common prefix that is different between
            // the nodes.
            const new_depth = depth + common_prefix_len;

            // Before mucking with the prefix keys, add the nodes to the new parent
            // using the first differentiating character in each's key.
            new_node.addChild(charAt(leaf_key, new_depth), NodeRef(T).init(.{ .leaf = leaf }));
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
            leaf_a: *NodeLeaf(T),
            leaf_b: *NodeLeaf(T),
            depth: u64,
        ) !NodeRef(T) {
            const new_node = NodeRef(T).init(.{ .node4 = try Node4(T).init(gpa) });
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

            new_node.addChild(charAt(leaf_a_key, new_depth), NodeRef(T).init(.{ .leaf = leaf_a }));
            new_node.addChild(charAt(leaf_b_key, new_depth), NodeRef(T).init(.{ .leaf = leaf_b }));

            return new_node;
        }
    };
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

test "mergeLeaves with small prefix" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const leaf_a = try NodeLeaf(usize).init(gpa, "ABC", 1);
    errdefer leaf_a.deinit(gpa);

    const leaf_b = try NodeLeaf(usize).init(gpa, "ABD", 1);
    errdefer leaf_b.deinit(gpa);

    const merged = try AdaptiveRadixTree(usize).splitLeaf(gpa, leaf_a, leaf_b, 0);
    defer merged.deinit(gpa);

    try testing.expectEqualStrings("AB", merged.getPrefix(0));
}

test "mergeLeaves with large prefix" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const Tree = AdaptiveRadixTree(usize);

    const leaf_a = try NodeLeaf(usize).init(gpa, "00000000001", 1);
    errdefer leaf_a.deinit(gpa);

    const leaf_b = try NodeLeaf(usize).init(gpa, "00000000002", 1);
    errdefer leaf_b.deinit(gpa);

    const merged = try Tree.splitLeaf(gpa, leaf_a, leaf_b, 0);
    defer merged.deinit(gpa);

    try testing.expectEqualStrings("0000000000", merged.getPrefix(0));
}

test "splitNode with small prefix" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const Tree = AdaptiveRadixTree(usize);

    const leaf_a = try NodeLeaf(usize).init(gpa, "ABC", 1);
    errdefer leaf_a.deinit(gpa);

    const leaf_b = try NodeLeaf(usize).init(gpa, "ABD", 1);
    errdefer leaf_b.deinit(gpa);

    const inner_a = try Tree.splitLeaf(gpa, leaf_a, leaf_b, 0);
    errdefer inner_a.deinit(gpa);

    try testing.expectEqualStrings("AB", inner_a.getPrefix(0));

    const leaf_c = try NodeLeaf(usize).init(gpa, "AE", 1);
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

    const leaf_a = try NodeLeaf(usize).init(gpa, "123456789_1/123456789_1", 1);
    errdefer leaf_a.deinit(gpa);

    const leaf_b = try NodeLeaf(usize).init(gpa, "123456789_1/123456789_2", 1);
    errdefer leaf_b.deinit(gpa);

    const inner_a = try Tree.splitLeaf(gpa, leaf_a, leaf_b, 0);
    errdefer inner_a.deinit(gpa);

    try testing.expectEqualStrings("123456789_1/123456789_", inner_a.getPrefix(0));

    const leaf_c = try NodeLeaf(usize).init(gpa, "123456789_2/123456789_1", 1);
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
