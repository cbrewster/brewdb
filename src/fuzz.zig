const std = @import("std");
const AdaptiveRadixTree = @import("art.zig").AdaptiveRadixTree;

export fn zig_fuzz_init() void {}

export fn zig_fuzz_test(buf: [*]u8, len: isize) void {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}).init;
    defer if (gpa_impl.deinit() == .leak) @panic("leaked memory in fuzz test");
    const gpa = gpa_impl.allocator();

    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    const src = buf[0..@intCast(len)];

    var tree = AdaptiveRadixTree(usize).init(gpa);
    defer tree.deinit();
    var ref = std.StringHashMap(usize).init(arena);

    var parser = InputParser.init(src);
    while (parser.hasMore()) {
        const op = parser.readOperation() orelse break;

        std.debug.print("op: {any}\n", .{op});

        switch (op.type) {
            .insert => {
                _ = tree.insert(op.key, op.value) catch continue;
                _ = ref.put(arena.dupe(u8, op.key) catch @panic("oom"), op.value) catch @panic("oom");
            },
            .get => {
                const actual = tree.get(op.key);
                const expected = ref.get(op.key);
                if (actual != expected) {
                    std.debug.print("Mismatch for key: {s}, expected: {any}, got: {any}\n", .{
                        op.key,
                        expected,
                        actual,
                    });
                    @panic("mismatch in get operation");
                }
            },
            .stress_test => {
                // Test edge cases that might trigger node growth/splits
                for (0..10) |i| {
                    const stress_key = std.fmt.allocPrint(arena, "{s}_{}", .{ op.key, i }) catch @panic("oom");
                    const value = op.value +% 1;

                    _ = tree.insert(stress_key, value) catch continue;
                    _ = ref.put(arena.dupe(u8, stress_key) catch @panic("oom"), value) catch @panic("oom");
                }
            },
        }
    }
}

const OpType = enum {
    insert,
    get,
    stress_test,

    // Helper function to get a nice string representation
    pub fn toString(self: OpType) []const u8 {
        return switch (self) {
            .insert => "INSERT",
            .get => "GET",
            .stress_test => "STRESS_TEST",
        };
    }

    // Get a colored representation (ANSI escape codes)
    pub fn toColorString(self: OpType) []const u8 {
        return switch (self) {
            .insert => "\x1b[32mINSERT\x1b[0m", // Green
            .get => "\x1b[34mGET\x1b[0m", // Blue
            .stress_test => "\x1b[35mSTRESS_TEST\x1b[0m", // Magenta
        };
    }
};
const Operation = struct {
    type: OpType,
    key: []const u8,
    value: u64,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;

        // Colored, compact format
        try writer.print("{s} \x1b[36m\"{s}\"\x1b[0m", .{ self.type.toColorString(), self.key });

        // Only show value for insert operations or if non-zero
        if (self.type == .insert or self.value != 0) {
            try writer.print(" \x1b[33m= {}\x1b[0m", .{self.value});
        }
    }
};

const InputParser = struct {
    data: []const u8,
    pos: usize,
    key_buffer: [256]u8,

    const SAFE_ASCII = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()-_=+[]{}|;:',.<>?/~`";
    const SAFE_ASCII_LEN = SAFE_ASCII.len;

    fn init(data: []const u8) InputParser {
        return InputParser{ .data = data, .pos = 0, .key_buffer = [_]u8{0} ** 256 };
    }

    fn hasMore(self: *InputParser) bool {
        return self.pos < self.data.len;
    }

    fn readByte(self: *InputParser) ?u8 {
        if (self.pos >= self.data.len) return null;
        const byte = self.data[self.pos];
        self.pos += 1;
        return byte;
    }

    fn readBytes(self: *InputParser, len: usize) ?[]const u8 {
        if (self.pos + len > self.data.len) return null;
        const bytes = self.data[self.pos .. self.pos + len];
        self.pos += len;
        return bytes;
    }

    fn mapToSafeAscii(raw_bytes: []const u8, output_buffer: []u8) void {
        for (raw_bytes, 0..) |byte, i| {
            output_buffer[i] = SAFE_ASCII[byte % SAFE_ASCII_LEN];
        }
    }

    fn readOperation(self: *InputParser) ?Operation {
        const type_byte = self.readByte() orelse return null;
        const op_type: OpType = switch (type_byte % 3) {
            0 => .insert,
            1 => .get,
            2 => .stress_test,
            else => unreachable,
        };

        // Read key length and ensure it's > 0
        const key_len_byte = self.readByte() orelse return null;
        const key_len = @max(1, @min(key_len_byte, self.key_buffer.len));

        // Read raw key bytes
        const raw_key = self.readBytes(key_len) orelse return null;

        // Map raw bytes to safe ASCII characters
        mapToSafeAscii(raw_key, self.key_buffer[0..key_len]);
        const key = self.key_buffer[0..key_len];

        const value_bytes = self.readBytes(8) orelse return Operation{
            .type = op_type,
            .key = key,
            .value = 0,
        };
        const value = std.mem.readInt(u64, value_bytes[0..8], .little);

        return Operation{
            .type = op_type,
            .key = key,
            .value = value,
        };
    }
};
