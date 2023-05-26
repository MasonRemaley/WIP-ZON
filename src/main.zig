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
    const parsed = switch (tags[node]) {
        .negation => self.parseSignedNumber(data[node].lhs, .negative),
        else => self.parseSignedNumber(node, .positive),
    };

    return switch (parsed.value) {
        .number => |number| switch (number) {
            .int => |int| switch (parsed.sign) {
                .positive => std.math.cast(T, int).?,
                .negative => switch (@typeInfo(T)) {
                    .Int => |int_type| switch (int_type.signedness) {
                        .signed => -std.math.cast(T, int).?,
                        .unsigned => unreachable,
                    },
                    .Float => -std.math.cast(T, int).?,
                    else => unreachable,
                },
            },
            else => unreachable,
        },
        .char => unreachable,
    };
}

// XXX: flatten the inner tagged unions?
const ParseSignedNumberValue = union(enum) {
    number: ParsedNumberLiteral,
    char: ParsedCharLiteral,
};

const ParseSignedNumberResult = struct {
    sign: Sign,
    value: ParseSignedNumberValue,
};

fn parseSignedNumber(self: *Parser, node: NodeIndex, sign: Sign) ParseSignedNumberResult {
    const main_tokens = self.ast.nodes.items(.main_token);
    const num_lit_token = main_tokens[node];
    const token_bytes = self.ast.tokenSlice(num_lit_token);
    const tags = self.ast.nodes.items(.tag);
    const value = switch (tags[node]) {
        .number_literal => ParseSignedNumberValue{
            .number = std.zig.number_literal.parseNumberLiteral(token_bytes),
        },
        .char_literal => ParseSignedNumberValue{
            .char = std.zig.string_literal.parseCharLiteral(token_bytes),
        },
        else => unreachable,
    };
    return .{
        .sign = sign,
        .value = value,
    };
}

test "parseInt" {
    const allocator = std.testing.allocator;
    try std.testing.expectEqual(parse(allocator, u8, "10"), 10);
    try std.testing.expectEqual(parse(allocator, i32, "-123"), -123);
    // XXX: ...
    // try std.testing.expectEqual(parse(allocator, u8, "'a'"), 'a');
    // try std.testing.expectEqual(parse(allocator, u8, "'z'"), 'z');
    // XXX: is char allowed for non u8s?
}
