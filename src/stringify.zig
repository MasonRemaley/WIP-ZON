const std = @import("std");

pub const Options = struct {
    indent_level: ?usize = 0,
    strings: bool = true,

    pub fn outputIndent(self: @This(), out_stream: anytype) @TypeOf(out_stream).Error!void {
        if (self.indent_level) |indent_level| {
            try out_stream.writeByte('\n');
            try out_stream.writeByteNTimes(' ', indent_level * 4);
        }
    }

    pub fn indent(self: *@This()) void {
        if (self.indent_level != null) {
            self.indent_level.? += 1;
        }
    }

    pub fn unindent(self: *@This(), out_stream: anytype) @TypeOf(out_stream).Error!void {
        if (self.indent_level != null) {
            self.indent_level.? -= 1;
            try self.outputIndent(out_stream);
        }
    }

    pub fn space(self: @This(), out_stream: anytype) @TypeOf(out_stream).Error!void {
        if (self.indent_level != null) {
            try out_stream.writeByte(' ');
        }
    }
};

pub fn stringify(value: anytype, options: Options, out_stream: anytype) !void {
    // TODO: keep in sync with parseExpr
    switch (@typeInfo(@TypeOf(value))) {
        .Bool => try out_stream.writeAll(if (value) "true" else "false"),
        .Float => try std.fmt.formatFloatScientific(value, .{}, out_stream),
        .Int => try std.fmt.formatIntValue(value, "", .{}, out_stream),
        // XXX: support parsing null as a type too!
        // XXX: support vectors? error sets?
        .Null => try out_stream.writeAll("null"),
        .Void => try out_stream.writeAll("{}"),
        .Optional => if (value) |payload|
            try stringify(payload, options, out_stream)
        else
            try out_stream.writeAll("null"),
        .Enum => if (std.enums.tagName(@TypeOf(value), value)) |name| {
            try out_stream.print(".{}", .{std.zig.fmtId(name)});
        } else {
            try out_stream.writeAll("@enumFromInt(");
            try std.fmt.formatIntValue(@intFromEnum(value), "", .{}, out_stream);
            try out_stream.writeByte(')');
        },
        .EnumLiteral => {
            try out_stream.writeByte('.');
            try out_stream.writeAll(@tagName(value));
        },
        .Array => try stringifyList(value, options, out_stream),
        .Pointer => |pointer| {
            const is_string = switch (pointer.size) {
                .One => switch (@typeInfo(pointer.child)) {
                    .Array => |array| array.child == u8,
                    else => failToStringifyType(@TypeOf(value)),
                },
                .Many, .Slice, .C => pointer.child == u8,
            };

            // Format as either a list or a string.
            if (is_string and options.strings) {
                try out_stream.print("\"{}\"", .{std.zig.fmtEscapes(value)});
            } else {
                try out_stream.writeByte('&');
                try stringifyList(value, options, out_stream);
            }
        },
        .Struct => |Struct| {
            var child_options = options;
            try out_stream.writeAll(".{");
            if (Struct.fields.len > 0) {
                child_options.indent();
                inline for (Struct.fields, 0..) |field, i| {
                    try child_options.outputIndent(out_stream);

                    if (!Struct.is_tuple) {
                        try out_stream.print(".{}", .{std.zig.fmtId(field.name)});

                        try child_options.space(out_stream);
                        try out_stream.writeByte('=');
                        try child_options.space(out_stream);
                    }

                    try stringify(@field(value, field.name), child_options, out_stream);

                    if (child_options.indent_level != null or i != Struct.fields.len - 1) {
                        try out_stream.writeByte(',');
                    }
                }
                try child_options.unindent(out_stream);
            }
            try out_stream.writeAll("}");
        },
        .Union => |Union| {
            if (Union.tag_type == null)
                failToStringifyType(@TypeOf(value));

            switch (value) {
                inline else => |payload, tag| {
                    if (@TypeOf(payload) == void) {
                        // XXX: can a union use a non exhaustive enum as its tag?
                        try out_stream.print(".{}", .{std.zig.fmtId(@tagName(tag))});
                    } else {
                        try out_stream.writeAll(".{");
                        try options.space(out_stream);

                        // XXX: can a union use a non exhaustive enum as its tag?
                        try out_stream.print(".{}", .{std.zig.fmtId(@tagName(tag))});

                        try options.space(out_stream);
                        try out_stream.writeByte('=');
                        try options.space(out_stream);

                        try stringify(payload, options, out_stream);
                        try options.space(out_stream);

                        try out_stream.writeAll("}");
                    }
                },
            }
        },

        else => failToStringifyType(@TypeOf(value)),
    }
}

fn stringifyList(value: anytype, options: Options, out_stream: anytype) !void {
    // XXX: better to have a single copy?
    var child_options = options;
    try out_stream.writeAll(".{");
    if (value.len > 0) {
        child_options.indent();
        for (value, 0..) |item, i| {
            try child_options.outputIndent(out_stream);
            try stringify(item, child_options, out_stream);
            if (child_options.indent_level != null or i != value.len - 1) {
                try out_stream.writeByte(',');
            }
        }
        try child_options.unindent(out_stream);
    }
    try out_stream.writeByte('}');
}

fn failToStringifyType(comptime T: type) noreturn {
    @compileError("Unable to stringify type '" ++ @typeName(T) ++ "'");
}

// XXX: supply stringifyAlloc like json does?
// XXX: explicit error type like output indent?
fn expectStringifyEqual(value: anytype, options: Options, expected: []const u8) !void {
    const gpa = std.testing.allocator;
    var stringified = std.ArrayList(u8).init(gpa);
    defer stringified.deinit();
    try stringify(value, options, stringified.writer());
    try std.testing.expectEqualStrings(expected, stringified.items);

    // XXX: test round tripping these here? (or option to do it--things like enum literals can't be round tripped!)
}

test "stringify basic" {
    // XXX: enum struct union union
    const Exhaustive = enum { A, B, @"while" };
    const NonExhaustive = enum(u8) { A, B, @"while", _ };

    // Primitives
    try expectStringifyEqual(true, .{}, "true");
    try expectStringifyEqual(false, .{}, "false");
    try expectStringifyEqual(@as(u32, 123), .{}, "123");
    try expectStringifyEqual(@as(f32, 0.5), .{}, "5.0e-01");
    try expectStringifyEqual(null, .{}, "null");
    try expectStringifyEqual({}, .{}, "{}");
    try expectStringifyEqual(@as(?bool, true), .{}, "true");
    try expectStringifyEqual(@as(?bool, null), .{}, "null");
    try expectStringifyEqual(Exhaustive.A, .{}, ".A");
    try expectStringifyEqual(NonExhaustive.B, .{}, ".B");
    try expectStringifyEqual(@as(NonExhaustive, @enumFromInt(3)), .{}, "@enumFromInt(3)");
    try expectStringifyEqual(.abc, .{}, ".abc");
    try expectStringifyEqual(Exhaustive.@"while", .{}, ".@\"while\"");
    try expectStringifyEqual(NonExhaustive.@"while", .{}, ".@\"while\"");

    // Arrays
    try expectStringifyEqual([_]u16{ 1, 2, 3 }, .{}, ".{\n    1,\n    2,\n    3,\n}");
    try expectStringifyEqual([_]u16{ 1, 2, 3 }, .{ .indent_level = null }, ".{1,2,3}");
    try expectStringifyEqual([_]u16{}, .{}, ".{}");
    try expectStringifyEqual([_]u16{}, .{ .indent_level = null }, ".{}");

    // XXX: what about sentinels and stuff, does it matter here?
    // Slices
    try expectStringifyEqual(@as([]const u16, &.{ 1, 2, 3 }), .{}, "&.{\n    1,\n    2,\n    3,\n}");
    try expectStringifyEqual(@as([]const u16, &.{ 1, 2, 3 }), .{ .indent_level = null }, "&.{1,2,3}");
    try expectStringifyEqual(@as([]const u16, &.{}), .{}, "&.{}");
    try expectStringifyEqual(@as([]const u16, &.{}), .{ .indent_level = null }, "&.{}");

    // Slices of chars
    try expectStringifyEqual("abc", .{ .indent_level = null, .strings = false }, "&.{97,98,99}");
    try expectStringifyEqual("ab\"c", .{ .indent_level = null }, "\"ab\\\"c\"");

    // Structs
    try expectStringifyEqual(.{}, .{}, ".{}");
    try expectStringifyEqual(.{}, .{ .indent_level = null }, ".{}");
    try expectStringifyEqual(
        .{ .x = @as(u8, 1), .y = @as(u8, 2), .z = .{ .@"hello world" = @as(u8, 3) } },
        .{ .indent_level = null },
        ".{.x=1,.y=2,.z=.{.@\"hello world\"=3}}",
    );
    try expectStringifyEqual(.{ .x = @as(u8, 1), .y = @as(u8, 2), .z = .{ .@"hello world" = @as(u8, 3) } }, .{},
        \\.{
        \\    .x = 1,
        \\    .y = 2,
        \\    .z = .{
        \\        .@"hello world" = 3,
        \\    },
        \\}
    );

    // XXX: if a list or struct only has one line, don't make a newline?
    // Tuples
    const Tuple = struct { u8, u8, struct { u8 } };
    try expectStringifyEqual(
        Tuple{ @as(u8, 1), @as(u8, 2), .{@as(u8, 3)} },
        .{ .indent_level = null },
        ".{1,2,.{3}}",
    );
    try expectStringifyEqual(Tuple{ @as(u8, 1), @as(u8, 2), .{@as(u8, 3)} }, .{},
        \\.{
        \\    1,
        \\    2,
        \\    .{
        \\        3,
        \\    },
        \\}
    );

    // Unions
    const Union = union(enum) {
        a: u8,
        @"hello world": u32,
        c: void,
        @"void space": void,
    };
    try expectStringifyEqual(Union{ .a = 1 }, .{ .indent_level = null }, ".{.a=1}");
    try expectStringifyEqual(Union{ .a = 1 }, .{}, ".{ .a = 1 }");
    try expectStringifyEqual(Union{ .@"hello world" = 1 }, .{ .indent_level = null }, ".{.@\"hello world\"=1}");
    try expectStringifyEqual(Union{ .@"hello world" = 1 }, .{}, ".{ .@\"hello world\" = 1 }");
    try expectStringifyEqual(@as(Union, .c), .{}, ".c");
    try expectStringifyEqual(@as(Union, .@"void space"), .{}, ".@\"void space\"");
}
