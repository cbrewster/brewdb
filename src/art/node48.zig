const std = @import("std");
const inner_node = @import("inner_node.zig");
const InnerNode = inner_node.InnerNode;
const NodeRef = @import("node.zig").NodeRef;
const Node256 = @import("node256.zig").Node256;
const NodeLeaf = @import("node_leaf.zig").NodeLeaf;
const util = @import("util.zig");
const pad = util.pad;

pub fn Node48(comptime T: type) type {
    return struct {
        const Self = @This();

        pub const MAX_CHILDREN: u16 = 48;

        const EMPTY = std.math.maxInt(u8);

        header: InnerNode(T) = .{ .kind = .node48 },
        key: [256]u8 = [_]u8{EMPTY} ** 256,
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
            for (self.key, 0..) |index, key| {
                if (index == EMPTY) continue;

                const child = self.children[index];
                try std.fmt.format(writer, "\n", .{});
                try pad(writer, indent);
                try std.fmt.format(writer, "\"{c}\": ", .{@as(u8, @intCast(key))});
                try child.?.print(writer, depth + 1 + self.header.prefix_len, indent);
            }
        }

        pub fn findChild(self: *Self, byte: u8) ?*?NodeRef(T) {
            std.debug.assert(self.header.num_children <= MAX_CHILDREN);

            const index = self.key[byte];
            if (index == EMPTY) return null;
            return &self.children[index];
        }

        pub fn addChild(self: *Self, byte: u8, child: NodeRef(T)) void {
            std.debug.assert(self.header.num_children < MAX_CHILDREN);
            // The key must not already exist in the node.
            std.debug.assert(self.key[byte] == EMPTY);

            const index = self.header.num_children;
            self.header.num_children += 1;
            self.children[index] = child;
            self.key[byte] = index;
        }

        pub fn grow(self: *Self, gpa: std.mem.Allocator) !*InnerNode(T) {
            std.debug.assert(self.header.num_children == MAX_CHILDREN);

            const new_node = try Node256(T).init(gpa);
            errdefer new_node.deinit(gpa);

            for (self.key, 0..) |index, key_byte| {
                if (index == EMPTY) continue;
                new_node.addChild(@as(u8, @intCast(key_byte)), self.children[index].?);
            }

            // Ownership of the children is transferred to the new node.
            self.children = [_]?NodeRef(T){null} ** MAX_CHILDREN;
            return &new_node.header;
        }

        pub fn min(self: *const Self) *const NodeLeaf(T) {
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
}
