const std = @import("std");
const inner_node = @import("inner_node.zig");
const InnerNode = inner_node.InnerNode;
const NodeRef = @import("node.zig").NodeRef;
const Node48 = @import("node48.zig").Node48;
const NodeLeaf = @import("node_leaf.zig").NodeLeaf;
const util = @import("util.zig");
const pad = util.pad;
const insertAt = util.insertAt;

pub fn Node16(comptime T: type) type {
    return struct {
        const Self = @This();

        pub const MAX_CHILDREN: u16 = 16;

        header: InnerNode(T) = .{ .kind = .node16 },
        key: [MAX_CHILDREN]u8 = [_]u8{0} ** MAX_CHILDREN,
        children: [MAX_CHILDREN]?NodeRef(T) = [_]?NodeRef(T){null} ** MAX_CHILDREN,

        pub fn init(gpa: std.mem.Allocator) !*Self {
            const node = try gpa.create(Self);
            node.* = .{};
            return node;
        }

        pub fn deinit(self: *Self, gpa: std.mem.Allocator) void {
            for (self.children[0..self.header.num_children]) |child| {
                if (child) |c| {
                    c.deinit(gpa);
                }
            }
            gpa.destroy(self);
        }

        pub fn isFull(self: *const Self) bool {
            return self.header.num_children == MAX_CHILDREN;
        }

        pub fn print(
            self: *const Self,
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

        pub fn findChild(self: *Self, byte: u8) ?*?NodeRef(T) {
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

        pub fn addChild(self: *Self, byte: u8, child: NodeRef(T)) void {
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
            insertAt(?NodeRef(T), self.children[0..self.header.num_children], index, child);
        }

        pub fn grow(self: *Self, gpa: std.mem.Allocator) !*InnerNode(T) {
            std.debug.assert(self.header.num_children == MAX_CHILDREN);

            const new_node = try Node48(T).init(gpa);
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

        pub fn min(self: *const Self) *const NodeLeaf(T) {
            std.debug.assert(self.header.num_children > 0);

            // TODO: Can probably use SIMD to speed up.
            const index = std.mem.indexOfMin(u8, self.key[0..self.header.num_children]);
            const child = self.children[index];
            std.debug.assert(child != null);
            return child.?.min();
        }
    };
}
