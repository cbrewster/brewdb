const std = @import("std");

/// TaggedPointer is a compile-time construct that allows you to create a pointer
/// which can hold a tag indicating the type of the pointed-to data.
///
/// The tag is stored in the least significant bits of the pointer which are
/// guaranteed to be unused due to the alignment of the pointed-to data.
///
/// The size of the tag is determined by the minimum alignment of all of the
/// possible types that can be pointed to.
///
/// In the future, we could utilize the most significant bits of the pointer
/// since those are likely to be unused on most platforms.
pub fn TaggedPointer(comptime fields: anytype) type {
    if (@typeInfo(@TypeOf(fields)) != .@"struct") {
        @compileError("TaggedPointer requires a struct type");
    }

    return struct {
        const Self = @This();
        pointer: usize,

        comptime {
            std.debug.assert(@sizeOf(@This()) == @sizeOf(usize));
        }

        const min_alignment = blk: {
            const f = std.meta.fields(@TypeOf(fields));
            var min: ?comptime_int = null;
            for (f) |field| {
                const field_type = @field(fields, field.name);
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

        const bit_count = std.math.log2_int(usize, min_alignment);
        const tag_mask: usize = (1 << bit_count) - 1;
        const ptr_mask: usize = ~tag_mask;

        const TagInt = blk: {
            const f = std.meta.fields(@TypeOf(fields));

            const tagInt = @Type(.{ .int = std.builtin.Type.Int{
                .bits = bit_count,
                .signedness = .unsigned,
            } });

            if (f.len > std.math.maxInt(tagInt)) {
                @compileError(std.fmt.comptimePrint(
                    "TaggedPointer with current alignment can only represent {} fields, but got {}",
                    .{ std.math.maxInt(tagInt), f.len },
                ));
            }

            break :blk tagInt;
        };

        const Tag = blk: {
            const f = std.meta.fields(@TypeOf(fields));
            var enum_fields: [f.len]std.builtin.Type.EnumField = undefined;
            var tag_to_type_mut: [f.len]type = undefined;
            for (f, 0..) |field, i| {
                enum_fields[i] = std.builtin.Type.EnumField{
                    .name = field.name,
                    .value = @as(TagInt, @intCast(i)),
                };
                tag_to_type_mut[i] = @field(fields, field.name);
            }

            break :blk @Type(.{ .@"enum" = std.builtin.Type.Enum{
                .fields = &enum_fields,
                .decls = &.{},
                .is_exhaustive = true,
                .tag_type = TagInt,
            } });
        };

        const tag_to_type = blk: {
            const f = std.meta.fields(@TypeOf(fields));
            var mapping: [f.len]type = undefined;
            for (f, 0..) |field, i| {
                mapping[i] = @field(fields, field.name);
            }
            break :blk mapping;
        };

        /// Initializes a TaggedPointer with the given tag and pointer.
        pub fn init(comptime tag: Tag, ptr: *tag_to_type[@intFromEnum(tag)]) Self {
            const tag_value = @as(TagInt, @intCast(@intFromEnum(tag)));
            std.debug.assert(tag_value & tag_mask == tag_value);
            return Self{ .pointer = @intFromPtr(ptr) | tag_value };
        }

        /// Returns the pointer to the data if the tag matches, or null if it does not.
        pub fn get(self: Self, comptime tag: Tag) ?*tag_to_type[@intFromEnum(tag)] {
            const tag_value = @as(TagInt, @intCast(@intFromEnum(tag)));
            std.debug.assert(tag_value & tag_mask == tag_value);
            if ((self.pointer & tag_mask) != tag_value) {
                return null;
            }
            return @ptrFromInt(self.pointer & ptr_mask);
        }

        /// Returns the pointer to the data assuming the tag matches.
        /// If the tag does not match, it will panic.
        pub fn as(self: Self, comptime tag: Tag) *tag_to_type[@intFromEnum(tag)] {
            return self.get(tag).?;
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

    const TaggedNode = TaggedPointer(.{
        .inner = InnerNode,
        .leaf = LeafNode,
    });
    var inner = InnerNode{ .inner = 42 };
    const node = TaggedNode.init(.inner, &inner);
    try std.testing.expectEqual(null, node.get(.leaf));
    try std.testing.expectEqual(&inner, node.get(.inner));
    const i: *InnerNode = node.get(.inner).?;
    try std.testing.expectEqual(42, i.inner);
}
