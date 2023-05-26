const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = std.zig.Ast;
const NodeIndex = std.zig.Ast.Node.Index;
const Type = std.builtin.Type;
const Base = std.zig.number_literal.Base;
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
        .Int => parser.parseNumber(T, root),
        else => unreachable,
    };
}

const Sign = enum { positive, negative };

fn parseNumber(self: *const Parser, comptime T: type, node: NodeIndex) !T {
    const tags = self.ast.nodes.items(.tag);
    const data = self.ast.nodes.items(.data);
    return switch (tags[node]) {
        .negation => try self.parseSignedNumber(T, data[node].lhs, .negative),
        else => try self.parseSignedNumber(T, node, .positive),
    };
}

fn parseSignedNumber(self: *const Parser, comptime T: type, node: NodeIndex, sign: Sign) !T {
    const main_tokens = self.ast.nodes.items(.main_token);
    const num_lit_token = main_tokens[node];
    const token_bytes = self.ast.tokenSlice(num_lit_token);
    const tags = self.ast.nodes.items(.tag);
    const value = switch (tags[node]) {
        .number_literal => {
            const number = std.zig.number_literal.parseNumberLiteral(token_bytes);
            switch (number) {
                .int => |int| return applySignToInt(T, sign, int),
                .big_int => |base| return self.parseBigInt(T, sign, node, base),
                else => unreachable,
            }
        },
        .char_literal => {
            const char = std.zig.string_literal.parseCharLiteral(token_bytes);
            return switch (char) {
                .success => |success| switch (sign) {
                    .positive => return std.math.cast(T, success).?,
                    .negative => unreachable,
                },
                else => unreachable,
            };
        },
        else => unreachable,
    };
    return .{
        .sign = sign,
        .value = value,
    };
}

fn applySignToInt(comptime T: type, sign: Sign, value: anytype) T {
    switch (sign) {
        .positive => return std.math.cast(T, value).?,
        .negative => {
            const signed_bits = switch (@typeInfo(T)) {
                .Int => |int_type| switch (int_type.signedness) {
                    .signed => int_type.bits,
                    .unsigned => unreachable,
                },
                else => unreachable,
            };
            const Positive = @Type(.{ .Int = .{
                .bits = signed_bits + 1,
                .signedness = .signed,
            } });
            const positive = std.math.cast(Positive, value).?;
            return std.math.cast(T, -positive).?;
        },
    }
}

// XXX: don't generate this except for cases that will actually need it?
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

test "parseInt" {
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
        @as(i65, 18446744073709551615),
        try parse(allocator, i65, "18446744073709551615"),
    );
    try std.testing.expectEqual(
        @as(i65, -18446744073709551616),
        try parse(allocator, i65, "-18446744073709551616"),
    );

    // XXX: ...
    // try std.testing.expectEqual(parse(allocator, i8, "-1.0"), -1);
    // XXX: is char allowed for non u8s?
}
