const std = @import("std");
const AdaptiveRadixTree = @import("art.zig").AdaptiveRadixTree;

fn generateSequentialKeys(allocator: std.mem.Allocator, count: usize) ![][]u8 {
    const keys = try allocator.alloc([]u8, count);
    for (keys, 0..) |*key, i| {
        key.* = try std.fmt.allocPrint(allocator, "key_{:010}", .{i});
    }
    return keys;
}

fn freeKeys(allocator: std.mem.Allocator, keys: [][]u8) void {
    for (keys) |key| {
        allocator.free(key);
    }
    allocator.free(keys);
}

fn benchmarkARTInsertSequential(allocator: std.mem.Allocator, num_keys: usize) !void {
    const keys = try generateSequentialKeys(allocator, num_keys);
    defer freeKeys(allocator, keys);

    var tree = AdaptiveRadixTree(usize).init(allocator);
    defer tree.deinit();

    const start = std.time.nanoTimestamp();
    for (keys, 0..) |key, i| {
        _ = try tree.insert(key, i);
    }
    const end = std.time.nanoTimestamp();

    const duration_ms = @as(f64, @floatFromInt(end - start)) / 1_000_000.0;
    std.debug.print("ART Insert {} keys: {d:.2} ms\n", .{ num_keys, duration_ms });
}

fn benchmarkStringHashMapInsertSequential(allocator: std.mem.Allocator, num_keys: usize) !void {
    const keys = try generateSequentialKeys(allocator, num_keys);
    defer freeKeys(allocator, keys);

    var map = std.StringHashMap(usize).init(allocator);
    defer map.deinit();

    const start = std.time.nanoTimestamp();
    for (keys, 0..) |key, i| {
        try map.put(key, i);
    }
    const end = std.time.nanoTimestamp();

    const duration_ms = @as(f64, @floatFromInt(end - start)) / 1_000_000.0;
    std.debug.print("StringHashMap Insert {} keys: {d:.2} ms\n", .{ num_keys, duration_ms });
}

pub fn main() !void {
    const allocator = std.heap.c_allocator;

    const num_keys = 100_000_000;

    std.debug.print("=== Performance Comparison ===\n", .{});
    try benchmarkARTInsertSequential(allocator, num_keys);
    try benchmarkStringHashMapInsertSequential(allocator, num_keys);
}

test {
    _ = @import("art.zig");
    _ = @import("bench.zig");
}
