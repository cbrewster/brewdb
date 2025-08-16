const std = @import("std");

pub fn main() !void {}

test {
    _ = @import("art.zig");
    _ = @import("bench.zig");
    _ = @import("tagged_pointer.zig");
    std.testing.refAllDeclsRecursive(@This());
}
