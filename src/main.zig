const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = std.zig.Ast;
const NodeIndex = std.zig.Ast.Node.Index;
const Type = std.builtin.Type;
const assert = std.debug.assert;
const ParsedNumberLiteral = std.zig.number_literal.Result;
const ParsedCharLiteral = std.zig.string_literal.ParsedCharLiteral;

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

    const root = ast.nodes.items(.data)[0].lhs;

    return switch (@typeInfo(T)) {
        .Int => parser.parseNumber(T, root),
        else => unreachable,
    };
}

const Sign = enum { positive, negative };

fn parseNumber(self: *Parser, comptime T: type, node: NodeIndex) T {
    const tags = self.ast.nodes.items(.tag);
    const data = self.ast.nodes.items(.data);
    return switch (tags[node]) {
        .negation => self.parseSignedNumber(T, data[node].lhs, .negative),
        else => self.parseSignedNumber(T, node, .positive),
    };
}

fn parseSignedNumber(self: *Parser, comptime T: type, node: NodeIndex, sign: Sign) T {
    const main_tokens = self.ast.nodes.items(.main_token);
    const num_lit_token = main_tokens[node];
    const token_bytes = self.ast.tokenSlice(num_lit_token);
    const tags = self.ast.nodes.items(.tag);
    const value = switch (tags[node]) {
        .number_literal => {
            const number = std.zig.number_literal.parseNumberLiteral(token_bytes);
            return switch (number) {
                .int => |int| applySignToInt(T, sign, int),
                else => unreachable,
            };
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

test "parseInt" {
    const allocator = std.testing.allocator;
    try std.testing.expectEqual(@as(u8, 10), try parse(allocator, u8, "10"));
    try std.testing.expectEqual(@as(i8, 127), try parse(allocator, i8, "127"));
    try std.testing.expectEqual(@as(i8, -128), try parse(allocator, i8, "-128"));
    try std.testing.expectEqual(@as(i32, -123), try parse(allocator, i32, "-123"));
    try std.testing.expectEqual(@as(u8, 'a'), try parse(allocator, u8, "'a'"));
    try std.testing.expectEqual(@as(u8, 'z'), try parse(allocator, u8, "'z'"));
    // XXX: ...
    // try std.testing.expectEqual(parse(allocator, i8, "-1.0"), -1);
    // XXX: is char allowed for non u8s?
}
