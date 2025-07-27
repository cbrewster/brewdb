const std = @import("std");
const AdaptiveRadixTree = @import("art.zig").AdaptiveRadixTree;

pub fn main() !void {
    const gpa = std.testing.allocator;
    var a = AdaptiveRadixTree(usize).init(gpa);
    defer a.deinit();
    for (0..10) |i| {
        const str = try std.fmt.allocPrint(gpa, "key-{}", .{i});
        defer gpa.free(str);
        _ = try a.insert(str, i);
        std.debug.print("root: {any}\n\n", .{a.root});
    }
    _ = try a.insert("A", 10);
    std.debug.print("root: {any}\n\n", .{a.root});
    _ = try a.insert("AB", 10);
    std.debug.print("root: {any}\n\n", .{a.root});
    const val = a.get("A");
    try std.testing.expectEqual(10, val);
    _ = try a.insert("AAB", 10);
    std.debug.print("root: {any}\n", .{a.root});
    _ = try a.insert("AAAB", 10);
    std.debug.print("root: {any}\n", .{a.root});
    _ = try a.insert("AAAAB", 10);
    std.debug.print("root: {any}\n", .{a.root});
    _ = try a.insert("AB", 10);
    std.debug.print("root: {any}\n", .{a.root});
    _ = try a.insert("B", 10);
    std.debug.print("root: {any}\n", .{a.root});
    // const val = a.get("A");
    // try std.testing.expectEqual(10, val);
}

test {
    _ = @import("art.zig");
}
