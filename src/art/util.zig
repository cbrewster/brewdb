const std = @import("std");

pub const END_OF_KEY = 0;

/// Returns the character at the given index in the slice.
/// If the index is equal to the length of the slice,
/// it returns an END_OF_KEY sentinel value.
pub fn charAt(slice: []const u8, index: usize) u8 {
    std.debug.assert(index <= slice.len);
    if (index == slice.len) {
        return END_OF_KEY;
    }
    return slice[index];
}

/// Inserts an element at the specified index in the slice.
/// All elements after the index are shifted to the right.
/// The rightmost element is discarded if the slice is full.
pub fn insertAt(comptime T: type, slice: []T, index: usize, element: T) void {
    std.debug.assert(index <= slice.len);
    if (index < slice.len - 1) {
        std.mem.copyBackwards(T, slice[index + 1 ..], slice[index .. slice.len - 1]);
    }
    slice[index] = element;
}

pub fn commonPrefix(a: []const u8, b: []const u8) []const u8 {
    return a[0..commonPrefixLength(a, b)];
}

pub fn commonPrefixLength(a: []const u8, b: []const u8) u64 {
    const min_len = @min(a.len, b.len);
    if (min_len == 0) {
        return 0;
    }

    var pos: u64 = 0;
    if (comptime std.simd.suggestVectorLength(u8)) |vector_len| {
        const chunk_size = @min(vector_len, 32);

        while (pos + chunk_size <= min_len) {
            const vec_a: @Vector(chunk_size, u8) = a[pos..][0..chunk_size].*;
            const vec_b: @Vector(chunk_size, u8) = b[pos..][0..chunk_size].*;

            const xor = vec_a ^ vec_b;
            const all_zero = @reduce(.Or, xor) == 0;

            if (all_zero) {
                pos += chunk_size;
            } else {
                break;
            }
        }
    }

    while (pos < min_len and a[pos] == b[pos]) {
        pos += 1;
    }

    return pos;
}

pub fn pad(writer: *std.io.Writer, amount: u64) !void {
    try writer.splatByteAll('\t', amount);
}

test commonPrefix {
    const testing = std.testing;

    // Test cases
    try testing.expectEqual(commonPrefixLength("hello", "help"), 3);
    try testing.expectEqual(commonPrefixLength("abc", "xyz"), 0);
    try testing.expectEqual(commonPrefixLength("same", "same"), 4);
    try testing.expectEqual(commonPrefixLength("", "anything"), 0);
    try testing.expectEqual(commonPrefixLength("test", ""), 0);

    // Test with longer strings to exercise SIMD path
    const long_a = "this is a very long string with many characters to test SIMD processing";
    const long_b = "this is a very long string with many different characters here";
    try testing.expectEqual(commonPrefixLength(long_a, long_b), 37);
    try testing.expectEqual(commonPrefixLength("hello", "help"), 3);
    try testing.expectEqual(commonPrefixLength("abcdefgh12345678", "abcdefgh87654321"), 8);
}
