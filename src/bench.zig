const std = @import("std");
const zbench = @import("zbench");
const art = @import("art.zig");

const config = struct {
    num_keys: usize = 1_000_000,
    key_len: usize = 32,
}{};

fn generateSequentialKeys(allocator: std.mem.Allocator, count: usize) ![][]u8 {
    const keys = try allocator.alloc([]u8, count);
    for (keys, 0..) |*key, i| {
        key.* = try std.fmt.allocPrint(allocator, "key_{:010}", .{i});
    }
    return keys;
}

fn generateUUIDKeys(allocator: std.mem.Allocator, count: usize) ![][]u8 {
    const keys = try allocator.alloc([]u8, count);
    var rng = std.Random.DefaultPrng.init(12345);

    for (keys) |*key| {
        var uuid: [16]u8 = undefined;
        rng.fill(&uuid);
        key.* = try std.fmt.allocPrint(allocator, "{x:0>8}-{x:0>4}-{x:0>4}-{x:0>4}-{x:0>12}", .{
            std.mem.readInt(u32, uuid[0..4], .big),
            std.mem.readInt(u16, uuid[4..6], .big),
            std.mem.readInt(u16, uuid[6..8], .big),
            std.mem.readInt(u16, uuid[8..10], .big),
            std.mem.readInt(u48, uuid[10..16], .big),
        });
    }
    return keys;
}

fn generateRandomKeys(allocator: std.mem.Allocator, count: usize, key_len: usize) ![][]u8 {
    const keys = try allocator.alloc([]u8, count);
    var rng = std.Random.DefaultPrng.init(54321);

    const charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

    for (keys) |*key| {
        key.* = try allocator.alloc(u8, key_len);
        for (key.*) |*c| {
            c.* = charset[rng.random().intRangeAtMost(usize, 0, charset.len - 1)];
        }
    }
    return keys;
}

fn freeKeys(allocator: std.mem.Allocator, keys: [][]u8) void {
    for (keys) |key| {
        allocator.free(key);
    }
    allocator.free(keys);
}

// ART Insert Benchmarks
fn benchmarkARTInsertSequential(allocator: std.mem.Allocator) void {
    const keys = generateSequentialKeys(allocator, config.num_keys) catch return;
    defer freeKeys(allocator, keys);

    var tree = art.AdaptiveRadixTree(usize).init(allocator);
    defer tree.deinit();

    for (keys, 0..) |key, i| {
        _ = tree.insert(key, i) catch return;
    }
}

fn benchmarkARTInsertUUID(allocator: std.mem.Allocator) void {
    const keys = generateUUIDKeys(allocator, config.num_keys) catch return;
    defer freeKeys(allocator, keys);

    var tree = art.AdaptiveRadixTree(usize).init(allocator);
    defer tree.deinit();

    for (keys, 0..) |key, i| {
        _ = tree.insert(key, i) catch return;
    }
}

fn benchmarkARTInsertRandom(allocator: std.mem.Allocator) void {
    const keys = generateRandomKeys(allocator, config.num_keys, config.key_len) catch return;
    defer freeKeys(allocator, keys);

    var tree = art.AdaptiveRadixTree(usize).init(allocator);
    defer tree.deinit();

    for (keys, 0..) |key, i| {
        _ = tree.insert(key, i) catch return;
    }
}

// StringHashMap Insert Benchmarks
fn benchmarkStringHashMapInsertSequential(allocator: std.mem.Allocator) void {
    const keys = generateSequentialKeys(allocator, config.num_keys) catch return;
    defer freeKeys(allocator, keys);

    var map = std.StringHashMap(usize).init(allocator);
    defer map.deinit();

    for (keys, 0..) |key, i| {
        map.put(key, i) catch return;
    }
}

fn benchmarkStringHashMapInsertUUID(allocator: std.mem.Allocator) void {
    const keys = generateUUIDKeys(allocator, config.num_keys) catch return;
    defer freeKeys(allocator, keys);

    var map = std.StringHashMap(usize).init(allocator);
    defer map.deinit();

    for (keys, 0..) |key, i| {
        map.put(key, i) catch return;
    }
}

fn benchmarkStringHashMapInsertRandom(allocator: std.mem.Allocator) void {
    const keys = generateRandomKeys(allocator, config.num_keys, config.key_len) catch return;
    defer freeKeys(allocator, keys);

    var map = std.StringHashMap(usize).init(allocator);
    defer map.deinit();

    for (keys, 0..) |key, i| {
        map.put(key, i) catch return;
    }
}

// ART Lookup Benchmarks
fn benchmarkARTLookupSequential(allocator: std.mem.Allocator) void {
    const keys = generateSequentialKeys(allocator, config.num_keys) catch return;
    defer freeKeys(allocator, keys);

    var tree = art.AdaptiveRadixTree(usize).init(allocator);
    defer tree.deinit();

    for (keys, 0..) |key, i| {
        _ = tree.insert(key, i) catch return;
    }

    for (keys) |key| {
        _ = tree.get(key);
    }
}

fn benchmarkARTLookupUUID(allocator: std.mem.Allocator) void {
    const keys = generateUUIDKeys(allocator, config.num_keys) catch return;
    defer freeKeys(allocator, keys);

    var tree = art.AdaptiveRadixTree(usize).init(allocator);
    defer tree.deinit();

    for (keys, 0..) |key, i| {
        _ = tree.insert(key, i) catch return;
    }

    for (keys) |key| {
        _ = tree.get(key);
    }
}

fn benchmarkARTLookupRandom(allocator: std.mem.Allocator) void {
    const keys = generateRandomKeys(allocator, config.num_keys, config.key_len) catch return;
    defer freeKeys(allocator, keys);

    var tree = art.AdaptiveRadixTree(usize).init(allocator);
    defer tree.deinit();

    for (keys, 0..) |key, i| {
        _ = tree.insert(key, i) catch return;
    }

    for (keys) |key| {
        _ = tree.get(key);
    }
}

// StringHashMap Lookup Benchmarks
fn benchmarkStringHashMapLookupSequential(allocator: std.mem.Allocator) void {
    const keys = generateSequentialKeys(allocator, config.num_keys) catch return;
    defer freeKeys(allocator, keys);

    var map = std.StringHashMap(usize).init(allocator);
    defer map.deinit();

    for (keys, 0..) |key, i| {
        map.put(key, i) catch return;
    }

    for (keys) |key| {
        _ = map.get(key);
    }
}

fn benchmarkStringHashMapLookupUUID(allocator: std.mem.Allocator) void {
    const keys = generateUUIDKeys(allocator, config.num_keys) catch return;
    defer freeKeys(allocator, keys);

    var map = std.StringHashMap(usize).init(allocator);
    defer map.deinit();

    for (keys, 0..) |key, i| {
        map.put(key, i) catch return;
    }

    for (keys) |key| {
        _ = map.get(key);
    }
}

fn benchmarkStringHashMapLookupRandom(allocator: std.mem.Allocator) void {
    const keys = generateRandomKeys(allocator, config.num_keys, config.key_len) catch return;
    defer freeKeys(allocator, keys);

    var map = std.StringHashMap(usize).init(allocator);
    defer map.deinit();

    for (keys, 0..) |key, i| {
        map.put(key, i) catch return;
    }

    for (keys) |key| {
        _ = map.get(key);
    }
}

test "ART vs StringHashMap Benchmarks" {
    var bench = zbench.Benchmark.init(std.heap.c_allocator, .{});
    defer bench.deinit();

    // Insert benchmarks
    try bench.add("ART Insert Sequential", benchmarkARTInsertSequential, .{});
    try bench.add("StringHashMap Insert Sequential", benchmarkStringHashMapInsertSequential, .{});

    try bench.add("ART Insert UUID", benchmarkARTInsertUUID, .{});
    try bench.add("StringHashMap Insert UUID", benchmarkStringHashMapInsertUUID, .{});

    try bench.add("ART Insert Random", benchmarkARTInsertRandom, .{});
    try bench.add("StringHashMap Insert Random", benchmarkStringHashMapInsertRandom, .{});

    // Lookup benchmarks
    try bench.add("ART Lookup Sequential", benchmarkARTLookupSequential, .{});
    try bench.add("StringHashMap Lookup Sequential", benchmarkStringHashMapLookupSequential, .{});

    try bench.add("ART Lookup UUID", benchmarkARTLookupUUID, .{});
    try bench.add("StringHashMap Lookup UUID", benchmarkStringHashMapLookupUUID, .{});

    try bench.add("ART Lookup Random", benchmarkARTLookupRandom, .{});
    try bench.add("StringHashMap Lookup Random", benchmarkStringHashMapLookupRandom, .{});

    try bench.run(std.io.getStdErr().writer());
}
