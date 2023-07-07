const std = @import("std");

pub const Options = struct {
    // XXX: clearer api?
    indent_level: ?usize = 0,

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
            try out_stream.writeByte('\n');
        }
    }
};

pub fn stringify(value: anytype, options: Options, out_stream: anytype) !void {
    // TODO: keep in sync with parseExpr
    switch (@typeInfo(@TypeOf(value))) {
        .Bool => try out_stream.writeAll(if (value) "true" else "false"),
        // XXX: decimal would be easier to read for cases like 0.5, is it not guaranteed to be exact?
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
            try out_stream.writeByte('.');
            try out_stream.writeAll(name);
        } else {
            try out_stream.writeAll("@enumFromInt(");
            try std.fmt.formatIntValue(@intFromEnum(value), "", .{}, out_stream);
            try out_stream.writeByte(')');
        },
        .EnumLiteral => {
            try out_stream.writeByte('.');
            try out_stream.writeAll(@tagName(value));
        },
        .Array => {
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
        },
        // .Pointer => self.stringifyPointer(options, value, out_stream),
        // .Struct => |Struct| if (Struct.is_tuple)
        //     self.stringifyTuple(options, value, out_stream)
        // else
        //     self.stringifyStruct(options, value, out_stream),
        // .Union => self.stringifyUnion(options, value, out_stream),

        else => failToStringifyType(@TypeOf(value)),
    }
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
    const Exhaustive = enum { A, B };
    const NonExhaustive = enum(u8) { A, B, _ };

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
    try expectStringifyEqual([_]u8{ 1, 2, 3 }, .{}, ".{\n    1,\n    2,\n    3,\n}");
    // XXX: skip trailing comma if no newlines?
    try expectStringifyEqual([_]u8{ 1, 2, 3 }, .{ .indent_level = null }, ".{1,2,3}");
    try expectStringifyEqual([_]u8{}, .{}, ".{}");
    try expectStringifyEqual([_]u8{}, .{ .indent_level = null }, ".{}");
}
