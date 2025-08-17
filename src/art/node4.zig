const std = @import("std");
const inner_node = @import("inner_node.zig");
const InnerNodeHeader = inner_node.InnerNodeHeader;
const NodeRef = @import("node.zig").NodeRef;
const Node16 = @import("node16.zig").Node16;
const NodeLeaf = @import("node_leaf.zig").NodeLeaf;

const util = @import("util.zig");
const pad = util.pad;
const insertAt = util.insertAt;

pub fn Node4(comptime T: type) type {
    return struct {
        const Self = @This();

        pub const MAX_CHILDREN: u16 = 4;

        header: InnerNodeHeader = .{},
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

            for (self.key[0..self.header.num_children], 0..) |key_byte, i| {
                if (key_byte == byte) {
                    return &self.children[i];
                }
            }
            return null;
        }

        pub fn addChild(self: *Self, byte: u8, child: NodeRef(T)) void {
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
            insertAt(?NodeRef(T), self.children[0..self.header.num_children], index, child);
        }

        pub fn grow(self: *Self, gpa: std.mem.Allocator) !NodeRef(T) {
            std.debug.assert(self.header.num_children == MAX_CHILDREN);

            const new_node = try Node16(T).init(gpa);
            errdefer new_node.deinit(gpa);

            for (
                self.key[0..self.header.num_children],
                self.children[0..self.header.num_children],
            ) |key_byte, child| {
                new_node.addChild(key_byte, child.?);
            }
            // Ownership of the children is transferred to the new node.
            self.header.num_children = 0;
            return NodeRef(T).init(.{ .node16 = new_node });
        }

        pub fn min(self: *const Self) *const NodeLeaf(T) {
            std.debug.assert(self.header.num_children > 0);

            const index = std.mem.indexOfMin(u8, self.key[0..self.header.num_children]);
            const child = self.children[index];
            std.debug.assert(child != null);
            return child.?.min();
        }
    };
}
