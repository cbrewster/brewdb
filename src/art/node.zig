const std = @import("std");

const util = @import("util.zig");
const commonPrefixLength = util.commonPrefixLength;
const TaggedPointer = @import("../tagged_pointer.zig").TaggedPointer;
const inner_node = @import("inner_node.zig");
const InnerNodeHeader = inner_node.InnerNodeHeader;
const Node4 = @import("node4.zig").Node4;
const Node16 = @import("node16.zig").Node16;
const Node48 = @import("node48.zig").Node48;
const Node256 = @import("node256.zig").Node256;
const NodeLeaf = @import("node_leaf.zig").NodeLeaf;

pub fn NodeRef(comptime T: type) type {
    return struct {
        const Self = @This();

        const TaggedNodePointer = TaggedPointer(union(enum) {
            node4: *Node4(T),
            node16: *Node16(T),
            node48: *Node48(T),
            node256: *Node256(T),
            leaf: *NodeLeaf(T),
        });

        ptr: TaggedNodePointer,

        pub fn init(value: TaggedNodePointer.Union) Self {
            return .{ .ptr = TaggedNodePointer.init(value) };
        }

        pub fn getHeader(self: Self) *InnerNodeHeader {
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
        pub fn getPrefix(self: Self, depth: usize) []const u8 {
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
        pub fn setPrefix(
            self: Self,
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
        pub fn checkPrefix(self: Self, key: []const u8, depth: u64) u64 {
            return commonPrefixLength(self.getPrefix(depth), key[depth..]);
        }

        /// Finds the child with the associated key byte.
        pub fn findChild(self: Self, byte: u8) ?*?Self {
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
        pub fn addChild(self: Self, byte: u8, child: Self) void {
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
        pub fn isFull(self: Self) bool {
            return self.getHeader().num_children ==
                switch (self.ptr.get()) {
                    .node4 => Node4(T).MAX_CHILDREN,
                    .node16 => Node16(T).MAX_CHILDREN,
                    .node48 => Node48(T).MAX_CHILDREN,
                    .node256 => @panic("isFull show not be called on a Node256"),
                    .leaf => @panic("isFull should not be called on a leaf node"),
                };
        }

        /// Grows the node into a larger size, (ie Node4 -> Node16).
        ///
        /// This creates a brand new node, the caller must deinit the old node.
        ///
        /// It is not valid to grow a NodeLeaf or Node256.
        pub fn grow(self: Self, gpa: std.mem.Allocator) !Self {
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
        pub fn min(self: Self) *const NodeLeaf(T) {
            return switch (self.ptr.get()) {
                .node4 => |node4| node4.min(),
                .node16 => |node16| node16.min(),
                .node48 => |node48| node48.min(),
                .node256 => |node256| node256.min(),
                .leaf => |leaf| leaf,
            };
        }

        pub fn deinit(self: Self, gpa: std.mem.Allocator) void {
            return switch (self.ptr.get()) {
                .node4 => |node4| node4.deinit(gpa),
                .node16 => |node16| node16.deinit(gpa),
                .node48 => |node48| node48.deinit(gpa),
                .node256 => |node256| node256.deinit(gpa),
                .leaf => |leaf| leaf.deinit(gpa),
            };
        }

        pub fn print(
            self: *const Self,
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
}
