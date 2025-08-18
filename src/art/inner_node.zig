const std = @import("std");

const util = @import("util.zig");
const commonPrefixLength = util.commonPrefixLength;
const Node4 = @import("node4.zig").Node4;
const Node16 = @import("node16.zig").Node16;
const Node48 = @import("node48.zig").Node48;
const Node256 = @import("node256.zig").Node256;
const NodeLeaf = @import("node_leaf.zig").NodeLeaf;
const NodeRef = @import("node.zig").NodeRef;

pub const InnerNodeHeader = struct {
    num_children: u8 = 0,
    partial_prefix_raw: [8]u8 = [_]u8{0} ** 8,
    prefix_len: u64 = 0,
};

pub fn InnerNode(comptime T: type) type {
    return struct {
        const Self = @This();

        const InnerNodeUnion = union(enum) {
            node4: *Node4(T),
            node16: *Node16(T),
            node48: *Node48(T),
            node256: *Node256(T),
        };

        const InnerNodeKind = @typeInfo(InnerNodeUnion).@"union".tag_type.?;

        kind: InnerNodeKind,

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

        fn getNode(self: *Self) InnerNodeUnion {
            switch (self.kind) {
                .node4 => return .{ .node4 = @fieldParentPtr("header", self) },
                .node16 => return .{ .node16 = @fieldParentPtr("header", self) },
                .node48 => return .{ .node48 = @fieldParentPtr("header", self) },
                .node256 => return .{ .node256 = @fieldParentPtr("header", self) },
            }
        }

        pub fn asNodeRef(self: *Self) NodeRef(T) {
            return NodeRef(T).init(.{ .inner = self });
        }

        pub fn deinit(self: *Self, gpa: std.mem.Allocator) void {
            switch (self.getNode()) {
                .node4 => |node4| node4.deinit(gpa),
                .node16 => |node16| node16.deinit(gpa),
                .node48 => |node48| node48.deinit(gpa),
                .node256 => |node256| node256.deinit(gpa),
            }
        }

        /// Returns the compressed key path stored inside this node.
        ///
        /// If the prefix is small enough, it can be stored inline in the node.
        ///
        /// Otherwise, only the length of the prefix is stored and the full prefix is determined
        /// by reading the full key of the first leaf node inside this subtree. The depth must
        /// be passed so we can strip the depth prefix from the leaf's key.
        pub fn getPrefix(self: *Self, depth: usize) []const u8 {
            if (self.prefix_len <= 8) return self.partial_prefix_raw[0..self.prefix_len];

            // The prefix could not fit inside the node, so instead we will find the full
            // key by grabbing the first child leaf node and indexing into it based on the
            // current depth and the prefix length stored in the header.
            //
            // We are gauranteed that every inner node in an ART will always have at least 2
            // child nodes, so we know that a leaf node is available at all times.

            const leaf = self.min();
            std.debug.assert(leaf.key.len >= depth + self.prefix_len);

            return leaf.key[depth .. depth + self.prefix_len];
        }

        /// Sets the node's prefix to perform path compression.
        ///
        /// Only the first 8 bytes of the prefix are stored in the node itself, larger prefixes
        /// are stored as a length within the inner node and are computed in `getPrefix` by
        /// getting the prefix from the first leaf's key.
        pub fn setPrefix(
            self: *Self,
            prefix: []const u8,
            comptime copy_forwards: bool,
        ) void {
            const copy_len = @min(prefix.len, 8);
            self.prefix_len = @intCast(prefix.len);
            if (copy_forwards) {
                std.mem.copyForwards(u8, self.partial_prefix_raw[0..copy_len], prefix[0..copy_len]);
            } else {
                @memcpy(self.partial_prefix_raw[0..copy_len], prefix[0..copy_len]);
            }
        }

        /// Gets the length of the prefix match between this inner node and a key at a specific depth.
        pub fn checkPrefix(self: *Self, key: []const u8, depth: u64) u64 {
            return commonPrefixLength(self.getPrefix(depth), key[depth..]);
        }

        /// Returns true if the node is full and needs to be grow to add a new child.
        pub fn isFull(self: *Self) bool {
            return switch (self.getNode()) {
                .node4 => |node4| node4.isFull(),
                .node16 => |node16| node16.isFull(),
                .node48 => |node48| node48.isFull(),
                .node256 => |node256| node256.isFull(),
            };
        }

        /// Grows the node into a larger size, (ie Node4 -> Node16).
        ///
        /// This creates a brand new node, the caller must deinit the old node.
        ///
        /// It is not valid to grow a NodeLeaf or Node256.
        pub fn grow(self: *Self, gpa: std.mem.Allocator) !*Self {
            const new_node = switch (self.getNode()) {
                .node4 => |node4| try node4.grow(gpa),
                .node16 => |node16| try node16.grow(gpa),
                .node48 => |node48| try node48.grow(gpa),
                .node256 => @panic("grow should not be called on a Node256"),
            };
            // No need to copy forward, slices do not overlap.
            new_node.prefix_len = self.prefix_len;
            @memcpy(&new_node.partial_prefix_raw, &self.partial_prefix_raw);
            return new_node;
        }

        /// Finds the child with the associated key byte.
        pub fn findChild(self: *Self, byte: u8) ?*?NodeRef(T) {
            return switch (self.getNode()) {
                .node4 => |node4| node4.findChild(byte),
                .node16 => |node16| node16.findChild(byte),
                .node48 => |node48| node48.findChild(byte),
                .node256 => |node256| node256.findChild(byte),
            };
        }

        /// Adds a child to the node. There must not already be a child in this
        /// node with the same key byte.
        pub fn addChild(self: *Self, byte: u8, child: NodeRef(T)) void {
            return switch (self.getNode()) {
                .node4 => |node4| node4.addChild(byte, child),
                .node16 => |node16| node16.addChild(byte, child),
                .node48 => |node48| node48.addChild(byte, child),
                .node256 => |node256| node256.addChild(byte, child),
            };
        }

        /// Returns the leaf node with the smallest key within this subtree.
        ///
        /// It is expected that every inner node will at least have 2 children.
        /// This property should always be preserved by ART implementation
        /// because path compression prevents inner nodes that only have a single child.
        pub fn min(self: *Self) *const NodeLeaf(T) {
            return switch (self.getNode()) {
                .node4 => |node4| node4.min(),
                .node16 => |node16| node16.min(),
                .node48 => |node48| node48.min(),
                .node256 => |node256| node256.min(),
            };
        }

        pub fn print(
            self: *Self,
            writer: anytype,
            depth: u64,
            indent: u64,
        ) anyerror!void {
            try std.fmt.format(writer, "Prefix \"{s}\" {s}", .{
                self.getPrefix(depth),
                @tagName(self.kind),
            });
            switch (self.getNode()) {
                .node4 => |node4| try node4.print(writer, depth, indent),
                .node16 => |node16| try node16.print(writer, depth, indent),
                .node48 => |node48| try node48.print(writer, depth, indent),
                .node256 => |node256| try node256.print(writer, depth, indent),
            }
        }
    };
}

test InnerNode {
    const testing = std.testing;
    const node = try Node4(usize).init(testing.allocator);
    defer node.deinit(testing.allocator);

    const inner_noder = &node.header;
    try testing.expectEqual(
        InnerNode(usize).InnerNodeUnion{ .node4 = node },
        inner_noder.getNode(),
    );
}
