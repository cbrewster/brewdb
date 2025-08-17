const std = @import("std");

pub fn main() !void {}

test {
    _ = @import("art/tree.zig");
    _ = @import("tagged_pointer.zig");
    std.testing.refAllDeclsRecursive(@This());
}
