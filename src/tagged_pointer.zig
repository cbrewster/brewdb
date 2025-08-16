const std = @import("std");

pub fn TaggedPointer(comptime T: type) type {
    const typeInfo = @typeInfo(T);
    if (typeInfo != .@"union" or typeInfo.@"union".tag_type == null) {
        @compileError("TaggedPointer requires a union(enum) type");
    }
    const unionInfo = typeInfo.@"union";

    const TagEnum = unionInfo.tag_type.?;
    const TagInt = @typeInfo(unionInfo.tag_type.?).@"enum".tag_type;

    return struct {
        const Self = @This();
        pointer: usize,

        pub const Union = T;

        comptime {
            std.debug.assert(@sizeOf(@This()) == @sizeOf(usize));

            const tagIntBits = @typeInfo(TagInt).int.bits;

            if (tagIntBits > TAG_BIT_COUNT) {
                @compileError(std.fmt.comptimePrint(
                    "TaggedPointer does not have enough low bits to represent the union(enum). Has {}, needs {}",
                    .{ tagIntBits, TAG_BIT_COUNT },
                ));
            }

            if (!@typeInfo(TagEnum).@"enum".is_exhaustive) {
                @compileError("TaggedPointer requires exhaustive union(enum)");
            }
        }

        const min_alignment = blk: {
            const fields = std.meta.fields(T);
            var min: ?comptime_int = null;
            for (fields) |field| {
                const field_type = field.type;
                if (@typeInfo(field_type) != .pointer) {
                    @compileError("TaggedPointer variant must always be a pointer");
                }

                if (min == null or std.meta.alignment(field_type) < min.?) {
                    min = std.meta.alignment(field_type);
                }
            }
            if (min == null) {
                @compileError("TaggedPointer requires at least one field");
            }

            if (!std.math.isPowerOfTwo(min.?)) {
                @compileError(std.fmt.comptimePrint(
                    "TaggedPointer requires a power of two alignment, got {}",
                    .{min.?},
                ));
            }

            break :blk min.?;
        };

        const TAG_BIT_COUNT = std.math.log2_int(usize, min_alignment);
        const TAG_MASK: usize = (1 << TAG_BIT_COUNT) - 1;
        const PTR_MASK: usize = ~TAG_MASK;

        /// Initializes a TaggedPointer with the given tag and pointer.
        pub fn init(value: T) Self {
            const tag_value = @intFromEnum(value);
            std.debug.assert(tag_value & TAG_MASK == tag_value);

            switch (value) {
                inline else => |ptr| {
                    std.debug.assert(@typeInfo(@TypeOf(ptr)) == .pointer);
                    return Self{ .pointer = @intFromPtr(ptr) | tag_value };
                },
            }
        }

        /// Returns the pointer represented as a tagged union.
        pub fn get(self: Self) T {
            const tag: TagEnum = @enumFromInt(self.pointer & TAG_MASK);
            const ptr = self.pointer & PTR_MASK;

            inline for (unionInfo.fields) |field| {
                if (tag == @field(TagEnum, field.name)) {
                    return @unionInit(T, field.name, @ptrFromInt(ptr));
                }
            }

            unreachable;
        }
    };
}

test TaggedPointer {
    const InnerNode = struct {
        inner: u64,
    };
    const LeafNode = struct {
        leaf: u64,
    };

    const TaggedNode = TaggedPointer(union(enum) {
        inner: *InnerNode,
        leaf: *LeafNode,
    });

    var inner = InnerNode{ .inner = 42 };
    const node = TaggedNode.init(.{ .inner = &inner });
    const i: *InnerNode = node.get().inner;
    try std.testing.expectEqual(42, i.inner);
}
