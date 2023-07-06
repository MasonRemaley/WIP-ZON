const std = @import("std");

pub const Options = struct {
    // XXX: fill in
};

pub fn stringify(value: anytype, options: Options, out_stream: anytype) !void {
    // TODO: keep in sync with parseExpr
    switch (@typeInfo(@TypeOf(value))) {
        .Bool => return out_stream.writeAll(if (value) "true" else "false"),
        // XXX: decimal would be easier to read for cases like 0.5, is it not guaranteed to be exact?
        .Float => return std.fmt.formatFloatScientific(value, .{}, out_stream),
        .Int => return std.fmt.formatIntValue(value, "", .{}, out_stream),
        // XXX: support parsing null as a type too!
        // XXX: support vectors? error sets?
        .Null => return out_stream.writeAll("null"),
        .Void => return out_stream.writeAll("{}"),
        .Optional => return if (value) |payload|
            stringify(payload, options, out_stream)
        else
            out_stream.writeAll("null"),
        // .Enum => self.stringifyEnumLiteral(options, value, out_stream),
        // .Pointer => self.stringifyPointer(options, value, out_stream),
        // .Array => self.stringifyArray(options, value, out_stream),
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

fn expectStringifyEqual(value: anytype, options: Options, expected: []const u8) !void {
    const gpa = std.testing.allocator;
    var stringified = std.ArrayList(u8).init(gpa);
    defer stringified.deinit();
    try stringify(value, options, stringified.writer());
    try std.testing.expectEqualStrings(expected, stringified.items);
}

test "stringify basic" {
    try expectStringifyEqual(true, .{}, "true");
    try expectStringifyEqual(false, .{}, "false");
    try expectStringifyEqual(@as(u32, 123), .{}, "123");
    try expectStringifyEqual(@as(f32, 0.5), .{}, "5.0e-01");
    try expectStringifyEqual(null, .{}, "null");
    try expectStringifyEqual({}, .{}, "{}");
    try expectStringifyEqual(@as(?bool, true), .{}, "true");
    try expectStringifyEqual(@as(?bool, null), .{}, "null");
}
