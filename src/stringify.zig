const std = @import("std");

pub const Options = struct {
    // XXX: fill in
};

pub fn stringify(value: anytype, options: Options, out_stream: anytype) !void {
    _ = options; // XXX: ...

    // TODO: keep in sync with parseExpr
    switch (@typeInfo(@TypeOf(value))) {
        .Bool => return out_stream.writeAll(if (value) "true" else "false"),
        // .Int, .Float => self.stringifyNumber(options, value, out_stream),
        // .Enum => self.stringifyEnumLiteral(options, value, out_stream),
        // .Pointer => self.stringifyPointer(options, value, out_stream),
        // .Array => self.stringifyArray(options, value, out_stream),
        // .Struct => |Struct| if (Struct.is_tuple)
        //     self.stringifyTuple(options, value, out_stream)
        // else
        //     self.stringifyStruct(options, value, out_stream),
        // .Union => self.stringifyUnion(options, value, out_stream),
        // .Optional => self.stringifyOptional(options, value, out_stream),
        // .Void => self.stringifyVoid(options, value, out_stream),

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
}
