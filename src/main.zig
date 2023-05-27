const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = std.zig.Ast;
const NodeIndex = std.zig.Ast.Node.Index;
const TokenIndex = std.zig.Ast.TokenIndex;
const Type = std.builtin.Type;
const Base = std.zig.number_literal.Base;
const FloatBase = std.zig.number_literal.FloatBase;
const assert = std.debug.assert;

const Parser = @This();

allocator: Allocator,
ast: Ast,
err: ?Error = null,

pub const Error = union(enum) {
    expected_type: struct {
        name: []const u8,
        token: TokenIndex,
    },
    cannot_represent: struct {
        name: []const u8,
        token: TokenIndex,
    },
};

pub fn Result(comptime T: type) type {
    return struct {
        value: union(enum) {
            success: T,
            type_error: Error,
            parse_error,
        },
        ast: Ast,

        fn deinit(self: *@This(), allocator: Allocator) void {
            self.ast.deinit(allocator);
            self.* = undefined;
        }

        // XXX: make a render errors function...
    };
}

pub fn parse(allocator: Allocator, comptime T: type, source: [:0]const u8) Allocator.Error!Result(T) {
    var ast = try std.zig.Ast.parse(allocator, source, .zon);
    errdefer ast.deinit(allocator);

    if (ast.errors.len != 0) {
        return .{
            .value = .parse_error,
            .ast = ast,
        };
    }

    var parser = Parser{
        .allocator = allocator,
        .ast = ast,
    };

    const data = ast.nodes.items(.data);
    const root = data[0].lhs;
    const success = parser.parseValue(T, root) catch |err| switch (err) {
        error.Type => return .{
            .value = .{ .type_error = parser.err.? },
            .ast = ast,
        },
    };

    return .{
        .value = .{ .success = success },
        .ast = ast,
    };
}

pub fn parseValue(self: *Parser, comptime T: type, node: NodeIndex) error{Type}!T {
    switch (@typeInfo(T)) {
        .Bool => return self.parseBool(node),
        .Int, .Float => return self.parseNumber(T, node),
        else => unreachable,
    }
}

fn fail(self: *Parser, err: Error) error{Type} {
    @setCold(true);
    assert(self.err == null);
    self.err = err;
    return error.Type;
}

fn failExpectedType(self: *Parser, comptime T: type, token: TokenIndex) error{Type} {
    @setCold(true);
    return self.fail(.{ .expected_type = .{
        .name = @typeName(T),
        .token = token,
    } });
}

fn failCannotRepresent(self: *Parser, comptime T: type, token: TokenIndex) error{Type} {
    @setCold(true);
    return self.fail(.{ .cannot_represent = .{
        .name = @typeName(T),
        .token = token,
    } });
}

fn parseThenFree(allocator: Allocator, comptime T: type, source: [:0]const u8) !T {
    var result = try parse(allocator, T, source);
    defer result.deinit(allocator);
    switch (result.value) {
        .parse_error => return error.Parse,
        .type_error => return error.Type,
        .success => |success| return success,
    }
}

fn parseBool(self: *Parser, node: NodeIndex) error{Type}!bool {
    const tags = self.ast.nodes.items(.tag);
    const main_tokens = self.ast.nodes.items(.main_token);
    const ident_token = main_tokens[node];
    switch (tags[node]) {
        .identifier => {
            const bytes = self.ast.tokenSlice(ident_token);
            if (std.mem.eql(u8, bytes, "true")) {
                return true;
            } else if (std.mem.eql(u8, bytes, "false")) {
                return false;
            }
        },
        else => {},
    }
    return self.failExpectedType(bool, ident_token);
}

test "parse bool" {
    const allocator = std.testing.allocator;

    // Correct floats
    try std.testing.expectEqual(true, try parseThenFree(allocator, bool, "true"));
    try std.testing.expectEqual(false, try parseThenFree(allocator, bool, "false"));

    // Errors
    try std.testing.expectError(error.Parse, parseThenFree(allocator, bool, "a b"));
    {
        var result = try parse(allocator, bool, " foo");
        defer result.deinit(allocator);
        try std.testing.expect(std.mem.eql(u8, result.value.type_error.expected_type.name, @typeName(bool)));
        const location = result.ast.tokenLocation(0, result.value.type_error.expected_type.token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 1,
            .line_start = 0,
            .line_end = 4,
        }, location);
    }
    {
        var result = try parse(allocator, bool, "123");
        defer result.deinit(allocator);
        try std.testing.expect(std.mem.eql(u8, result.value.type_error.expected_type.name, @typeName(bool)));
        const location = result.ast.tokenLocation(0, result.value.type_error.expected_type.token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 3,
        }, location);
    }
}

const Sign = enum { positive, negative };

fn parseNumber(self: *Parser, comptime T: type, node: NodeIndex) error{Type}!T {
    const tags = self.ast.nodes.items(.tag);
    const data = self.ast.nodes.items(.data);
    return switch (tags[node]) {
        .negation => try self.parseNumberWithSign(T, data[node].lhs, .negative),
        else => try self.parseNumberWithSign(T, node, .positive),
    };
}

fn parseNumberWithSign(
    self: *Parser,
    comptime T: type,
    node: NodeIndex,
    sign: Sign,
) error{Type}!T {
    const tags = self.ast.nodes.items(.tag);
    switch (tags[node]) {
        .number_literal => return self.parseNumberLiteral(T, node, sign),
        .char_literal => return self.parseCharLiteral(T, node, sign),
        else => {
            const main_tokens = self.ast.nodes.items(.main_token);
            const token = main_tokens[node];
            return self.failExpectedType(T, token);
        },
    }
}

fn parseNumberLiteral(self: *Parser, comptime T: type, node: NodeIndex, sign: Sign) error{Type}!T {
    const main_tokens = self.ast.nodes.items(.main_token);
    const num_lit_token = main_tokens[node];
    const token_bytes = self.ast.tokenSlice(num_lit_token);
    const number = std.zig.number_literal.parseNumberLiteral(token_bytes);
    switch (number) {
        .int => |int| return self.applySignToInt(T, num_lit_token, sign, int),
        .big_int => |base| return self.parseBigNumber(T, sign, node, base),
        .float => return self.parseFloat(T, sign, node),
        else => unreachable,
    }
}

fn applySignToInt(self: *Parser, comptime T: type, token: TokenIndex, sign: Sign, int: anytype) error{Type}!T {
    switch (sign) {
        .positive => switch (@typeInfo(T)) {
            .Int => return std.math.cast(T, int) orelse
                self.failCannotRepresent(T, token),
            .Float => return @intToFloat(T, int),
            else => @compileError("expected numeric type"),
        },
        .negative => switch (@typeInfo(T)) {
            .Int => |int_type| switch (int_type.signedness) {
                .signed => {
                    const Positive = @Type(.{ .Int = .{
                        .bits = int_type.bits + 1,
                        .signedness = .signed,
                    } });
                    if (int > std.math.maxInt(T) + 1) {
                        return self.failCannotRepresent(T, token);
                    }
                    const positive = @intCast(Positive, int);
                    return @intCast(T, -positive);
                },
                .unsigned => return self.failCannotRepresent(T, token),
            },
            .Float => return -@intToFloat(T, int),
            else => @compileError("expected numeric type"),
        },
    }
}

fn parseBigNumber(
    self: *Parser,
    comptime T: type,
    sign: Sign,
    node: NodeIndex,
    base: Base,
) error{Type}!T {
    switch (@typeInfo(T)) {
        .Int => return self.parseBigInt(T, sign, node, base),
        // TODO: passing in f128 to work around possible float parsing bug
        .Float => return @floatCast(T, try self.parseFloat(f128, sign, node)),
        else => unreachable,
    }
}

// XXX: test overflow errors
fn parseBigInt(self: *Parser, comptime T: type, sign: Sign, node: NodeIndex, base: Base) error{Type}!T {
    const main_tokens = self.ast.nodes.items(.main_token);
    const token = main_tokens[node];
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
        result = std.math.mul(T, result, @intCast(T, @enumToInt(base))) catch
            return self.failCannotRepresent(T, token);
        switch (sign) {
            .positive => result = std.math.add(T, result, @intCast(T, d)) catch
                return self.failCannotRepresent(T, token),
            .negative => result = std.math.sub(T, result, @intCast(T, d)) catch
                return self.failCannotRepresent(T, token),
        }
    }
    return result;
}

fn parseFloat(
    self: *Parser,
    comptime T: type,
    sign: Sign,
    node: NodeIndex,
) error{Type}!T {
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
        const main_tokens = self.ast.nodes.items(.main_token);
        const token = main_tokens[node];
        return floatToInt(T, result) orelse
            self.failCannotRepresent(T, token);
    }
}

fn parseCharLiteral(self: *Parser, comptime T: type, node: NodeIndex, sign: Sign) error{Type}!T {
    const main_tokens = self.ast.nodes.items(.main_token);
    const num_lit_token = main_tokens[node];
    const token_bytes = self.ast.tokenSlice(num_lit_token);
    const char = std.zig.string_literal.parseCharLiteral(token_bytes).success;
    return self.applySignToInt(T, num_lit_token, sign, char);
}

// TODO: move to std.math?
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
    try std.testing.expectEqual(@as(u8, 10), try parseThenFree(allocator, u8, "10"));
    try std.testing.expectEqual(@as(i16, 24), try parseThenFree(allocator, i16, "24"));
    try std.testing.expectEqual(@as(i14, -4), try parseThenFree(allocator, i14, "-4"));
    try std.testing.expectEqual(@as(i32, -123), try parseThenFree(allocator, i32, "-123"));

    // Test limits
    try std.testing.expectEqual(@as(i8, 127), try parseThenFree(allocator, i8, "127"));
    try std.testing.expectEqual(@as(i8, -128), try parseThenFree(allocator, i8, "-128"));

    // Test characters
    try std.testing.expectEqual(@as(u8, 'a'), try parseThenFree(allocator, u8, "'a'"));
    try std.testing.expectEqual(@as(u8, 'z'), try parseThenFree(allocator, u8, "'z'"));

    // Test big integers
    try std.testing.expectEqual(
        @as(u65, 36893488147419103231),
        try parseThenFree(allocator, u65, "36893488147419103231"),
    );
    try std.testing.expectEqual(
        @as(u65, 36893488147419103231),
        try parseThenFree(allocator, u65, "368934_881_474191032_31"),
    );

    // Test big integer limits
    try std.testing.expectEqual(
        @as(i66, 36893488147419103231),
        try parseThenFree(allocator, i66, "36893488147419103231"),
    );
    try std.testing.expectEqual(
        @as(i66, -36893488147419103232),
        try parseThenFree(allocator, i66, "-36893488147419103232"),
    );

    // Test parsing whole number floats as integers
    try std.testing.expectEqual(@as(i8, -1), try parseThenFree(allocator, i8, "-1.0"));
    try std.testing.expectEqual(@as(i8, 123), try parseThenFree(allocator, i8, "123.0"));

    // Test non-decimal integers
    try std.testing.expectEqual(@as(i16, 0xff), try parseThenFree(allocator, i16, "0xff"));
    try std.testing.expectEqual(@as(i16, -0xff), try parseThenFree(allocator, i16, "-0xff"));
    try std.testing.expectEqual(@as(i16, 0o77), try parseThenFree(allocator, i16, "0o77"));
    try std.testing.expectEqual(@as(i16, -0o77), try parseThenFree(allocator, i16, "-0o77"));
    try std.testing.expectEqual(@as(i16, 0b11), try parseThenFree(allocator, i16, "0b11"));
    try std.testing.expectEqual(@as(i16, -0b11), try parseThenFree(allocator, i16, "-0b11"));

    // Test non-decimal big integers
    try std.testing.expectEqual(@as(u65, 0x1ffffffffffffffff), try parseThenFree(
        allocator,
        u65,
        "0x1ffffffffffffffff",
    ));
    try std.testing.expectEqual(@as(i66, 0x1ffffffffffffffff), try parseThenFree(
        allocator,
        i66,
        "0x1ffffffffffffffff",
    ));
    try std.testing.expectEqual(@as(i66, -0x1ffffffffffffffff), try parseThenFree(
        allocator,
        i66,
        "-0x1ffffffffffffffff",
    ));
    try std.testing.expectEqual(@as(u65, 0x1ffffffffffffffff), try parseThenFree(
        allocator,
        u65,
        "0o3777777777777777777777",
    ));
    try std.testing.expectEqual(@as(i66, 0x1ffffffffffffffff), try parseThenFree(
        allocator,
        i66,
        "0o3777777777777777777777",
    ));
    try std.testing.expectEqual(@as(i66, -0x1ffffffffffffffff), try parseThenFree(
        allocator,
        i66,
        "-0o3777777777777777777777",
    ));
    try std.testing.expectEqual(@as(u65, 0x1ffffffffffffffff), try parseThenFree(
        allocator,
        u65,
        "0b11111111111111111111111111111111111111111111111111111111111111111",
    ));
    try std.testing.expectEqual(@as(i66, 0x1ffffffffffffffff), try parseThenFree(
        allocator,
        i66,
        "0b11111111111111111111111111111111111111111111111111111111111111111",
    ));
    try std.testing.expectEqual(@as(i66, -0x1ffffffffffffffff), try parseThenFree(
        allocator,
        i66,
        "-0b11111111111111111111111111111111111111111111111111111111111111111",
    ));

    // Failinig to parse as int
    {
        var result = try parse(allocator, u8, "true");
        defer result.deinit(allocator);
        try std.testing.expect(std.mem.eql(u8, result.value.type_error.expected_type.name, @typeName(u8)));
        const location = result.ast.tokenLocation(0, result.value.type_error.expected_type.token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 4,
        }, location);
    }

    // Failing because an int is out of range
    {
        var result = try parse(allocator, u8, "256");
        defer result.deinit(allocator);
        try std.testing.expect(std.mem.eql(u8, result.value.type_error.cannot_represent.name, @typeName(u8)));
        const location = result.ast.tokenLocation(0, result.value.type_error.cannot_represent.token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 3,
        }, location);
    }

    // Failing because a negative int is out of range
    {
        var result = try parse(allocator, i8, "-129");
        defer result.deinit(allocator);
        try std.testing.expect(std.mem.eql(u8, result.value.type_error.cannot_represent.name, @typeName(i8)));
        const location = result.ast.tokenLocation(0, result.value.type_error.cannot_represent.token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            // XXX: should point at the minus sign...?
            .column = 1,
            .line_start = 0,
            .line_end = 4,
        }, location);
    }

    // Failing because an unsigned int is negative
    {
        var result = try parse(allocator, u8, "-1");
        defer result.deinit(allocator);
        try std.testing.expect(std.mem.eql(u8, result.value.type_error.cannot_represent.name, @typeName(u8)));
        const location = result.ast.tokenLocation(0, result.value.type_error.cannot_represent.token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            // XXX: should point at the minus sign?
            .column = 1,
            .line_start = 0,
            .line_end = 2,
        }, location);
    }

    // Failing because a float is non-whole
    {
        var result = try parse(allocator, u8, "1.5");
        defer result.deinit(allocator);
        try std.testing.expect(std.mem.eql(u8, result.value.type_error.cannot_represent.name, @typeName(u8)));
        const location = result.ast.tokenLocation(0, result.value.type_error.cannot_represent.token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 3,
        }, location);
    }

    // Failing because a float is negative
    {
        var result = try parse(allocator, u8, "-1.0");
        defer result.deinit(allocator);
        try std.testing.expect(std.mem.eql(u8, result.value.type_error.cannot_represent.name, @typeName(u8)));
        const location = result.ast.tokenLocation(0, result.value.type_error.cannot_represent.token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            // XXX: column should be 0
            .column = 1,
            .line_start = 0,
            .line_end = 4,
        }, location);
    }
}

test "parse float" {
    const allocator = std.testing.allocator;

    // Test decimals
    try std.testing.expectEqual(@as(f16, 0.5), try parseThenFree(allocator, f16, "0.5"));
    try std.testing.expectEqual(@as(f32, 123.456), try parseThenFree(allocator, f32, "123.456"));
    try std.testing.expectEqual(@as(f64, -123.456), try parseThenFree(allocator, f64, "-123.456"));
    try std.testing.expectEqual(@as(f128, 42.5), try parseThenFree(allocator, f128, "42.5"));

    // Test whole numbers with and without decimals
    try std.testing.expectEqual(@as(f16, 5.0), try parseThenFree(allocator, f16, "5.0"));
    try std.testing.expectEqual(@as(f16, 5.0), try parseThenFree(allocator, f16, "5"));
    try std.testing.expectEqual(@as(f32, -102), try parseThenFree(allocator, f32, "-102.0"));
    try std.testing.expectEqual(@as(f32, -102), try parseThenFree(allocator, f32, "-102"));

    // Test characters and negated characters
    try std.testing.expectEqual(@as(f32, 'a'), try parseThenFree(allocator, f32, "'a'"));
    try std.testing.expectEqual(@as(f32, 'z'), try parseThenFree(allocator, f32, "'z'"));
    try std.testing.expectEqual(@as(f32, -'z'), try parseThenFree(allocator, f32, "-'z'"));

    // Test big integers
    try std.testing.expectEqual(
        @as(f32, 36893488147419103231),
        try parseThenFree(allocator, f32, "36893488147419103231"),
    );
    try std.testing.expectEqual(
        @as(f32, -36893488147419103231),
        try parseThenFree(allocator, f32, "-36893488147419103231"),
    );
    try std.testing.expectEqual(@as(f128, 0x1ffffffffffffffff), try parseThenFree(
        allocator,
        f128,
        "0x1ffffffffffffffff",
    ));
    try std.testing.expectEqual(@as(f32, 0x1ffffffffffffffff), try parseThenFree(
        allocator,
        f32,
        "0x1ffffffffffffffff",
    ));

    // Exponents, underscores
    try std.testing.expectEqual(@as(f32, 123.0E+77), try parseThenFree(allocator, f32, "12_3.0E+77"));

    // Hexadecimal
    try std.testing.expectEqual(@as(f32, 0x103.70p-5), try parseThenFree(allocator, f32, "0x103.70p-5"));
    try std.testing.expectEqual(@as(f32, -0x103.70), try parseThenFree(allocator, f32, "-0x103.70"));
    try std.testing.expectEqual(
        @as(f32, 0x1234_5678.9ABC_CDEFp-10),
        try parseThenFree(allocator, f32, "0x1234_5678.9ABC_CDEFp-10"),
    );
}

// TODO: zig float parsing bug example
// test "bug" {
//     const float: f32 = 0xffffffffffffffff;
//     const parsed = try std.fmt.parseFloat(f32, "0xffffffffffffffff.0p0");
//     try std.testing.expectEqual(float, parsed);
// }
