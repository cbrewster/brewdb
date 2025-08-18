const std = @import("std");
const inner_node = @import("inner_node.zig");
const InnerNode = inner_node.InnerNode;
const NodeRef = @import("node.zig").NodeRef;
const NodeLeaf = @import("node_leaf.zig").NodeLeaf;
const util = @import("util.zig");
const pad = util.pad;

pub fn Node256(comptime T: type) type {
    return struct {
        const Self = @This();
        const MAX_CHILDREN: u16 = 256;

        header: InnerNode(T) = .{ .kind = .node256 },
        children: [MAX_CHILDREN]?NodeRef(T) = [_]?NodeRef(T){null} ** MAX_CHILDREN,

        pub fn init(gpa: std.mem.Allocator) !*Self {
            const node = try gpa.create(Self);
            node.* = .{};
            return node;
        }

        pub fn deinit(self: *Self, gpa: std.mem.Allocator) void {
            for (self.children) |child| {
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
            for (self.children, 0..) |child, key| {
                if (child == null) continue;

                try std.fmt.format(writer, "\n", .{});
                try pad(writer, indent);
                try std.fmt.format(writer, "\"{c}\": ", .{@as(u8, @intCast(key))});
                try child.?.print(writer, depth + 1 + self.header.prefix_len, indent);
            }
        }

        pub fn findChild(self: *Self, byte: u8) ?*?NodeRef(T) {
            std.debug.assert(self.header.num_children <= MAX_CHILDREN);

            return &self.children[byte];
        }

        pub fn addChild(self: *Self, byte: u8, child: NodeRef(T)) void {
            std.debug.assert(self.header.num_children < MAX_CHILDREN);
            // The key must not already exist in the node.
            std.debug.assert(self.children[byte] == null);

            self.header.num_children += 1;
            self.children[byte] = child;
        }

        pub fn min(self: *const Self) *const NodeLeaf(T) {
            std.debug.assert(self.header.num_children > 0);

            for (self.children) |child| {
                if (child != null) return child.?.min();
            }

            unreachable;
        }
    };
}
