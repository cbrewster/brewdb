const std = @import("std");

pub fn NodeLeaf(comptime T: type) type {
    return struct {
        const Self = @This();
        key: []const u8,
        value: T,

        pub fn init(gpa: std.mem.Allocator, key: []const u8, value: T) !*Self {
            const key_clone = try gpa.dupe(u8, key);
            errdefer gpa.free(key_clone);

            const node = try gpa.create(Self);
            node.* = .{
                .key = key_clone,
                .value = value,
            };
            return node;
        }

        pub fn deinit(self: *Self, gpa: std.mem.Allocator) void {
            gpa.free(self.key);
            gpa.destroy(self);
        }

        pub fn print(self: *const Self, writer: *std.Io.Writer) std.io.Writer.Error!void {
            try writer.print("\"{s}\" = {any}", .{ self.key, self.value });
        }
    };
}
