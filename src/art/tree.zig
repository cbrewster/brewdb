const std = @import("std");
const NodeRef = @import("node.zig").NodeRef;
const NodeLeaf = @import("node_leaf.zig").NodeLeaf;
const Node4 = @import("node4.zig").Node4;
const InnerNode = @import("inner_node.zig").InnerNode;
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
            switch (node.ptr.get()) {
                .leaf => |node_leaf| {
                    if (std.mem.eql(u8, node_leaf.key, key)) {
                        return node_leaf;
                    }
                    return null;
                },
                .inner => |node_inner| {
                    if (node_inner.checkPrefix(key, depth) != node_inner.prefix_len) {
                        return null;
                    }
                    const new_depth = depth + node_inner.prefix_len;
                    const child = node_inner.findChild(charAt(key, new_depth)) orelse return null;
                    return search(child.*, key, new_depth + 1);
                },
            }
        }

        pub fn format(
            self: *const Self,
            writer: *std.io.Writer,
        ) error{WriteFailed}!void {
            if (self.root) |root| {
                try root.print(writer, 0, 0);
            } else {
                try writer.print("<empty tree>", .{});
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

            switch (node.ptr.get()) {
                .leaf => |node_leaf| {
                    // If the keys of the two leaves match, the old leaf will be deleted and replaced by the new node.
                    if (std.mem.eql(u8, node_leaf.key, key)) {
                        const old_value = node_leaf.value;
                        node_leaf.value = value;
                        return old_value;
                    }

                    const leaf = try NodeLeaf(T).init(self.gpa, key, value);
                    errdefer leaf.deinit(self.gpa);

                    const new_node = try splitLeaf(
                        self.gpa,
                        node_leaf,
                        leaf,
                        depth,
                    );
                    maybe_node.* = new_node.asNodeRef();
                    return null;
                },
                .inner => |node_inner| {
                    // Since we store prefixes in the inner nodes, we need to check to see
                    // if this node requires the inner node to be split.

                    const match_len = node_inner.checkPrefix(key, depth);
                    if (match_len != node_inner.prefix_len) {
                        const leaf = try NodeLeaf(T).init(self.gpa, key, value);
                        errdefer leaf.deinit(self.gpa);

                        const new_node = try splitInner(
                            self.gpa,
                            node_inner,
                            leaf,
                            depth,
                            match_len,
                        );
                        maybe_node.* = new_node.asNodeRef();
                        return null;
                    }

                    // Continue traversing down the tree, if we find another child node, we will
                    // recurse back into this function until we hit a leaf or a hole.
                    const new_depth = depth + node_inner.prefix_len;
                    if (node_inner.findChild(charAt(key, new_depth))) |next| {
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

                    if (node_inner.isFull()) {
                        const new_node = try node_inner.grow(self.gpa);
                        errdefer new_node.deinit(self.gpa);
                        new_node.addChild(charAt(key, new_depth), NodeRef(T).init(.{ .leaf = leaf }));
                        node_inner.deinit(self.gpa);
                        maybe_node.* = new_node.asNodeRef();
                        return null;
                    }

                    node_inner.addChild(charAt(key, new_depth), NodeRef(T).init(.{ .leaf = leaf }));
                    return null;
                },
            }
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
            node: *InnerNode(T),
            leaf: *NodeLeaf(T),
            depth: u64,
            common_prefix_len: u64,
        ) !*InnerNode(T) {
            const new_node = &(try Node4(T).init(gpa)).header;
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
            new_node.addChild(charAt(node_partial_key, common_prefix_len), node.asNodeRef());

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
        ) !*InnerNode(T) {
            const new_node = &(try Node4(T).init(gpa)).header;
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

test {
    _ = @import("fuzz.zig");
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
    try testing.expectEqualStrings("", inner_a.getPrefix(inner_b.prefix_len));
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
        inner_a.getPrefix(inner_b.prefix_len + 1),
    );
}
