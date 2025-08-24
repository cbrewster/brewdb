const std = @import("std");
const AdaptiveRadixTree = @import("tree.zig").AdaptiveRadixTree;

test "fuzz AdapativeRadixTree" {
    const Context = struct {
        fn testOne(context: @This(), input: []const u8) !void {
            _ = context;

            var gpa_impl = std.heap.GeneralPurposeAllocator(.{}).init;
            defer if (gpa_impl.deinit() == .leak) @panic("leaked memory in fuzz test");
            const gpa = gpa_impl.allocator();

            var arena_impl = std.heap.ArenaAllocator.init(gpa);
            defer arena_impl.deinit();
            const arena = arena_impl.allocator();

            var tree = AdaptiveRadixTree(usize).init(gpa);
            defer tree.deinit();
            var ref = std.StringHashMap(usize).init(arena);

            var parser = InputParser.init(input);
            while (parser.hasMore()) {
                const op = parser.readOperation() orelse break;

                switch (op) {
                    .insert => |data| {
                        _ = tree.insert(data.key, data.value) catch continue;
                        _ = ref.put(arena.dupe(u8, data.key) catch @panic("oom"), data.value) catch @panic("oom");
                    },
                    .get => |data| {
                        const actual = tree.get(data.key);
                        const expected = ref.get(data.key);
                        if (actual != expected) {
                            std.debug.print("Mismatch for key: {s}, expected: {any}, got: {any}\n", .{
                                data.key,
                                expected,
                                actual,
                            });
                            std.debug.print("Tree: {f}\n", .{tree});
                            @panic("mismatch in get operation");
                        }
                    },
                }
            }
        }

        const Operation = union(enum) {
            insert: struct { key: []const u8, value: u64 },
            get: struct { key: []const u8 },

            pub fn format(
                self: @This(),
                writer: *std.io.Writer,
            ) std.io.Writer.Error!void {
                switch (self) {
                    .insert => |data| {
                        try writer.print("\x1b[32mINSERT\x1b[0m \x1b[36m\"{s}\"\x1b[0m \x1b[33m= {}\x1b[0m", .{ data.key, data.value });
                    },
                    .get => |data| {
                        try writer.print("\x1b[34mGET\x1b[0m \x1b[36m\"{s}\"\x1b[0m", .{data.key});
                    },
                }
            }
        };

        const InputParser = struct {
            data: []const u8,
            pos: usize,
            key_buffer: [256]u8,

            const SAFE_ASCII = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()-_=+[]{}|;:',.<>?/~`";

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
                    output_buffer[i] = SAFE_ASCII[byte % SAFE_ASCII.len];
                }
            }

            fn readOperation(self: *InputParser) ?Operation {
                const type_byte = self.readByte() orelse return null;
                const is_insert = (type_byte % 2) == 0;

                // Read key length and ensure it's > 0
                const key_len_byte = self.readByte() orelse return null;
                const key_len = @max(1, @min(key_len_byte, self.key_buffer.len));

                // Read raw key bytes
                const raw_key = self.readBytes(key_len) orelse return null;

                // Map raw bytes to safe ASCII characters
                mapToSafeAscii(raw_key, self.key_buffer[0..key_len]);
                const key = self.key_buffer[0..key_len];

                if (is_insert) {
                    const value_bytes = self.readBytes(8) orelse return Operation{
                        .insert = .{ .key = key, .value = 0 },
                    };
                    const value = std.mem.readInt(u64, value_bytes[0..8], .little);

                    return Operation{
                        .insert = .{ .key = key, .value = value },
                    };
                } else {
                    return Operation{
                        .get = .{ .key = key },
                    };
                }
            }
        };
    };

    try std.testing.fuzz(Context{}, Context.testOne, .{});
}
