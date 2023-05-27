const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = std.zig.Ast;
const NodeIndex = std.zig.Ast.Node.Index;
const Type = std.builtin.Type;
const Base = std.zig.number_literal.Base;
const FloatBase = std.zig.number_literal.FloatBase;
const assert = std.debug.assert;

const Parser = @This();

allocator: Allocator,
ast: Ast,

pub fn parse(allocator: Allocator, comptime T: type, source: [:0]const u8) !T {
    var ast = try std.zig.Ast.parse(allocator, source, .zon);
    defer ast.deinit(allocator);
    if (ast.errors.len != 0) {
        unreachable;
    }

    var parser = Parser{
        .allocator = allocator,
        .ast = ast,
    };

    const data = ast.nodes.items(.data);
    const root = data[0].lhs;

    return switch (@typeInfo(T)) {
        .Int, .Float => parser.parseNumber(T, root),
        else => unreachable,
    };
}

const Sign = enum { positive, negative };

fn parseNumber(self: *const Parser, comptime T: type, node: NodeIndex) !T {
    const tags = self.ast.nodes.items(.tag);
    const data = self.ast.nodes.items(.data);
    return switch (tags[node]) {
        .negation => try self.parseNumberWithSign(T, data[node].lhs, .negative),
        else => try self.parseNumberWithSign(T, node, .positive),
    };
}

fn parseNumberWithSign(self: *const Parser, comptime T: type, node: NodeIndex, sign: Sign) !T {
    const tags = self.ast.nodes.items(.tag);
    const value = switch (tags[node]) {
        .number_literal => return self.parseNumberLiteral(T, node, sign),
        .char_literal => return self.parseCharLiteral(T, node, sign),
        else => unreachable,
    };
    return .{
        .sign = sign,
        .value = value,
    };
}

fn parseNumberLiteral(self: *const Parser, comptime T: type, node: NodeIndex, sign: Sign) T {
    const main_tokens = self.ast.nodes.items(.main_token);
    const num_lit_token = main_tokens[node];
    const token_bytes = self.ast.tokenSlice(num_lit_token);
    const number = std.zig.number_literal.parseNumberLiteral(token_bytes);
    switch (number) {
        .int => |int| return applySignToInt(T, sign, int),
        .big_int => |base| return self.parseBigNumber(T, sign, node, base),
        .float => return self.parseFloat(T, sign, node),
        else => unreachable,
    }
}

fn applySignToInt(comptime T: type, sign: Sign, int: anytype) T {
    switch (sign) {
        .positive => switch (@typeInfo(T)) {
            .Int => return std.math.cast(T, int).?,
            .Float => return @intToFloat(T, int),
            else => unreachable,
        },
        .negative => switch (@typeInfo(T)) {
            .Int => |int_type| switch (int_type.signedness) {
                .signed => {
                    const Positive = @Type(.{ .Int = .{
                        .bits = int_type.bits + 1,
                        .signedness = .signed,
                    } });
                    const positive = std.math.cast(Positive, int).?;
                    return std.math.cast(T, -positive).?;
                },
                .unsigned => unreachable,
            },
            .Float => return -@intToFloat(T, int),
            else => unreachable,
        },
    }
}

fn parseBigNumber(
    self: *const Parser,
    comptime T: type,
    sign: Sign,
    node: NodeIndex,
    base: Base,
) T {
    switch (@typeInfo(T)) {
        .Int => return self.parseBigInt(T, sign, node, base),
        // TODO: passing in f128 to work around possible float parsing bug
        .Float => return @floatCast(T, self.parseFloat(f128, sign, node)),
        else => unreachable,
    }
}

fn parseBigInt(self: *const Parser, comptime T: type, sign: Sign, node: NodeIndex, base: Base) T {
    const data = self.ast.nodes.items(.data);
    const bytes_node = switch (sign) {
        .positive => data[node].lhs,
        .negative => node,
    };
    const bytes = self.ast.tokenSlice(bytes_node);
    const prefix_offset = @as(u8, 2) * @boolToInt(base != .decimal);
    var result: T = 0;
    for (bytes[prefix_offset..]) |char| {
        if (char == '_') continue;
        const d = std.fmt.charToDigit(char, @enumToInt(base)) catch unreachable;
        result *= @intCast(T, @enumToInt(base));
        switch (sign) {
            .positive => result += @intCast(T, d),
            .negative => result -= @intCast(T, d),
        }
    }
    return result;
}

fn parseFloat(
    self: *const Parser,
    comptime T: type,
    sign: Sign,
    node: NodeIndex,
) T {
    const Float = switch (@typeInfo(T)) {
        .Float => T,
        .Int => f128,
        else => unreachable,
    };
    const data = self.ast.nodes.items(.data);
    const bytes_node = switch (sign) {
        .positive => data[node].lhs,
        .negative => node,
    };
    const bytes = self.ast.tokenSlice(bytes_node);
    const unsigned_float = std.fmt.parseFloat(Float, bytes) catch unreachable;
    const result = switch (sign) {
        .negative => -unsigned_float,
        .positive => unsigned_float,
    };
    if (T == Float) {
        return result;
    } else {
        return floatToInt(T, result).?;
    }
}

fn parseCharLiteral(self: *const Parser, comptime T: type, node: NodeIndex, sign: Sign) T {
    const main_tokens = self.ast.nodes.items(.main_token);
    const num_lit_token = main_tokens[node];
    const token_bytes = self.ast.tokenSlice(num_lit_token);
    const char_result = std.zig.string_literal.parseCharLiteral(token_bytes);
    const char = switch (char_result) {
        .success => |char| char,
        else => unreachable,
    };
    return applySignToInt(T, sign, char);
}

fn floatToInt(comptime T: type, value: anytype) ?T {
    switch (@typeInfo(@TypeOf(value))) {
        .Float, .ComptimeFloat => {},
        else => @compileError(@typeName(@TypeOf(value)) ++ " is not a floating point type"),
    }
    switch (@typeInfo(T)) {
        .Int, .ComptimeInt => {},
        else => @compileError(@typeName(T) ++ " is not an integer type"),
    }

    if (T == comptime_int or (value > std.math.maxInt(T) or value < std.math.minInt(T))) {
        return null;
    }

    if (std.math.isNan(value) or std.math.trunc(value) != value) {
        return null;
    }

    return @floatToInt(T, value);
}

test "floatToInt" {
    // Valid conversions
    try std.testing.expectEqual(@as(u8, 10), floatToInt(u8, 10.0).?);
    try std.testing.expectEqual(@as(i8, -123), floatToInt(i8, @as(f64, -123.0)).?);
    try std.testing.expectEqual(@as(i16, 45), floatToInt(i16, @as(f128, 45.0)).?);
    try std.testing.expectEqual(@as(comptime_int, 10), comptime floatToInt(i16, @as(f32, 10.0)).?);
    try std.testing.expectEqual(
        @as(comptime_int, 10),
        comptime floatToInt(i16, @as(comptime_float, 10.0)).?,
    );
    try std.testing.expectEqual(@as(u8, 5), floatToInt(u8, @as(comptime_float, 5.0)).?);

    // Out of range
    try std.testing.expectEqual(@as(?u4, null), floatToInt(u4, @as(f32, 16.0)));
    try std.testing.expectEqual(@as(?i4, null), floatToInt(i4, -17.0));
    try std.testing.expectEqual(@as(?u8, null), floatToInt(u8, -2.0));

    // Not a whole number
    try std.testing.expectEqual(@as(?u8, null), floatToInt(u8, 0.5));
    try std.testing.expectEqual(@as(?i8, null), floatToInt(i8, 0.01));

    // Infinity and NaN
    try std.testing.expectEqual(@as(?u8, null), floatToInt(u8, std.math.inf(f32)));
    try std.testing.expectEqual(@as(?u8, null), floatToInt(u8, -std.math.inf(f32)));
    try std.testing.expectEqual(@as(?u8, null), floatToInt(u8, std.math.nan(f32)));
    try std.testing.expectEqual(
        @as(?comptime_int, null),
        comptime floatToInt(comptime_int, std.math.inf(f32)),
    );
}

test "parse int" {
    const allocator = std.testing.allocator;

    // Test various numbers and types
    try std.testing.expectEqual(@as(u8, 10), try parse(allocator, u8, "10"));
    try std.testing.expectEqual(@as(i16, 24), try parse(allocator, i16, "24"));
    try std.testing.expectEqual(@as(i14, -4), try parse(allocator, i14, "-4"));
    try std.testing.expectEqual(@as(i32, -123), try parse(allocator, i32, "-123"));

    // Test limits
    try std.testing.expectEqual(@as(i8, 127), try parse(allocator, i8, "127"));
    try std.testing.expectEqual(@as(i8, -128), try parse(allocator, i8, "-128"));

    // Test characters
    try std.testing.expectEqual(@as(u8, 'a'), try parse(allocator, u8, "'a'"));
    try std.testing.expectEqual(@as(u8, 'z'), try parse(allocator, u8, "'z'"));

    // Test big integers
    try std.testing.expectEqual(
        @as(u65, 36893488147419103231),
        try parse(allocator, u65, "36893488147419103231"),
    );
    try std.testing.expectEqual(
        @as(u65, 36893488147419103231),
        try parse(allocator, u65, "368934_881_474191032_31"),
    );

    // Test big integer limits
    try std.testing.expectEqual(
        @as(i66, 36893488147419103231),
        try parse(allocator, i66, "36893488147419103231"),
    );
    try std.testing.expectEqual(
        @as(i66, -36893488147419103232),
        try parse(allocator, i66, "-36893488147419103232"),
    );

    // Test parsing whole number floats as integers
    try std.testing.expectEqual(@as(i8, -1), try parse(allocator, i8, "-1.0"));
    try std.testing.expectEqual(@as(i8, 123), try parse(allocator, i8, "123.0"));

    // Test non-decimal integers
    try std.testing.expectEqual(@as(i16, 0xff), try parse(allocator, i16, "0xff"));
    try std.testing.expectEqual(@as(i16, -0xff), try parse(allocator, i16, "-0xff"));
    try std.testing.expectEqual(@as(i16, 0o77), try parse(allocator, i16, "0o77"));
    try std.testing.expectEqual(@as(i16, -0o77), try parse(allocator, i16, "-0o77"));
    try std.testing.expectEqual(@as(i16, 0b11), try parse(allocator, i16, "0b11"));
    try std.testing.expectEqual(@as(i16, -0b11), try parse(allocator, i16, "-0b11"));

    // Test non-decimal big integers
    try std.testing.expectEqual(@as(u65, 0x1ffffffffffffffff), try parse(
        allocator,
        u65,
        "0x1ffffffffffffffff",
    ));
    try std.testing.expectEqual(@as(i66, 0x1ffffffffffffffff), try parse(
        allocator,
        i66,
        "0x1ffffffffffffffff",
    ));
    try std.testing.expectEqual(@as(i66, -0x1ffffffffffffffff), try parse(
        allocator,
        i66,
        "-0x1ffffffffffffffff",
    ));
    try std.testing.expectEqual(@as(u65, 0x1ffffffffffffffff), try parse(
        allocator,
        u65,
        "0o3777777777777777777777",
    ));
    try std.testing.expectEqual(@as(i66, 0x1ffffffffffffffff), try parse(
        allocator,
        i66,
        "0o3777777777777777777777",
    ));
    try std.testing.expectEqual(@as(i66, -0x1ffffffffffffffff), try parse(
        allocator,
        i66,
        "-0o3777777777777777777777",
    ));
    try std.testing.expectEqual(@as(u65, 0x1ffffffffffffffff), try parse(
        allocator,
        u65,
        "0b11111111111111111111111111111111111111111111111111111111111111111",
    ));
    try std.testing.expectEqual(@as(i66, 0x1ffffffffffffffff), try parse(
        allocator,
        i66,
        "0b11111111111111111111111111111111111111111111111111111111111111111",
    ));
    try std.testing.expectEqual(@as(i66, -0x1ffffffffffffffff), try parse(
        allocator,
        i66,
        "-0b11111111111111111111111111111111111111111111111111111111111111111",
    ));
}

test "parse float" {
    const allocator = std.testing.allocator;

    // Test decimals
    try std.testing.expectEqual(@as(f16, 0.5), try parse(allocator, f16, "0.5"));
    try std.testing.expectEqual(@as(f32, 123.456), try parse(allocator, f32, "123.456"));
    try std.testing.expectEqual(@as(f64, -123.456), try parse(allocator, f64, "-123.456"));
    try std.testing.expectEqual(@as(f128, 42.5), try parse(allocator, f128, "42.5"));

    // Test whole numbers with and without decimals
    try std.testing.expectEqual(@as(f16, 5.0), try parse(allocator, f16, "5.0"));
    try std.testing.expectEqual(@as(f16, 5.0), try parse(allocator, f16, "5"));
    try std.testing.expectEqual(@as(f32, -102), try parse(allocator, f32, "-102.0"));
    try std.testing.expectEqual(@as(f32, -102), try parse(allocator, f32, "-102"));

    // Test characters and negated characters
    try std.testing.expectEqual(@as(f32, 'a'), try parse(allocator, f32, "'a'"));
    try std.testing.expectEqual(@as(f32, 'z'), try parse(allocator, f32, "'z'"));
    try std.testing.expectEqual(@as(f32, -'z'), try parse(allocator, f32, "-'z'"));

    // Test big integers
    try std.testing.expectEqual(
        @as(f32, 36893488147419103231),
        try parse(allocator, f32, "36893488147419103231"),
    );
    try std.testing.expectEqual(
        @as(f32, -36893488147419103231),
        try parse(allocator, f32, "-36893488147419103231"),
    );
    try std.testing.expectEqual(@as(f128, 0x1ffffffffffffffff), try parse(
        allocator,
        f128,
        "0x1ffffffffffffffff",
    ));
    try std.testing.expectEqual(@as(f32, 0x1ffffffffffffffff), try parse(
        allocator,
        f32,
        "0x1ffffffffffffffff",
    ));

    // Exponents, underscores
    try std.testing.expectEqual(@as(f32, 123.0E+77), try parse(allocator, f32, "12_3.0E+77"));

    // Hexadecimal
    try std.testing.expectEqual(@as(f32, 0x103.70p-5), try parse(allocator, f32, "0x103.70p-5"));
    try std.testing.expectEqual(@as(f32, -0x103.70), try parse(allocator, f32, "-0x103.70"));
    try std.testing.expectEqual(
        @as(f32, 0x1234_5678.9ABC_CDEFp-10),
        try parse(allocator, f32, "0x1234_5678.9ABC_CDEFp-10"),
    );
}

// TODO: zig float parsing bug example
// test "bug" {
//     const float: f32 = 0xffffffffffffffff;
//     const parsed = try std.fmt.parseFloat(f32, "0xffffffffffffffff.0p0");
//     try std.testing.expectEqual(float, parsed);
// }
