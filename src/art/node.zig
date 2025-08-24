const std = @import("std");

const TaggedPointer = @import("../tagged_pointer.zig").TaggedPointer;
const inner_node = @import("inner_node.zig");
const InnerNode = inner_node.InnerNode;
const NodeLeaf = @import("node_leaf.zig").NodeLeaf;

pub fn NodeRef(comptime T: type) type {
    return struct {
        const Self = @This();

        const TaggedNodePointer = TaggedPointer(union(enum) {
            inner: *InnerNode(T),
            leaf: *NodeLeaf(T),
        });

        ptr: TaggedNodePointer,

        pub fn init(value: TaggedNodePointer.Union) Self {
            return .{ .ptr = TaggedNodePointer.init(value) };
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

        pub fn deinit(self: Self, gpa: std.mem.Allocator) void {
            return switch (self.ptr.get()) {
                .inner => |inner| inner.deinit(gpa),
                .leaf => |leaf| leaf.deinit(gpa),
            };
        }

        pub fn min(self: Self) *const NodeLeaf(T) {
            return switch (self.ptr.get()) {
                .inner => |inner| inner.min(),
                .leaf => |leaf| leaf,
            };
        }

        pub fn print(
            self: *const Self,
            writer: *std.Io.Writer,
            depth: u64,
            indent: u64,
        ) std.io.Writer.Error!void {
            switch (self.ptr.get()) {
                .inner => |inner| try inner.print(writer, depth, indent + 1),
                .leaf => |leaf| try leaf.print(writer),
            }

            if (indent == 0) {
                try writer.print("\n", .{});
            }
        }
    };
}
