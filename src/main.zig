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
ast: *const Ast,
err: ?*Error,

// XXX: naming?
pub const Error = union(enum) {
    success: void,
    expected_type: struct {
        name: []const u8,
        node: NodeIndex,
    },
    cannot_represent: struct {
        name: []const u8,
        node: NodeIndex,
    },
};

pub fn Result(comptime T: type) type {
    // TODO: make a render errors function...
    return union(enum) {
        success: T,
        err: Error,

        // TODO: unimplemented...
        fn deinit(self: *@This(), _: Allocator) void {
            self.* = undefined;
        }
    };
}

pub fn parse(allocator: Allocator, comptime T: type, ast: *const Ast, err: ?*Error) error{ OutOfMemory, Type }!T {
    var parser = Parser{
        .allocator = allocator,
        .ast = ast,
        .err = err,
    };
    const data = ast.nodes.items(.data);
    // TODO: why lhs here?
    const root = data[0].lhs;
    return parser.parseExpr(T, root);
}

pub fn parseSlice(allocator: Allocator, comptime T: type, source: [:0]const u8) error{ OutOfMemory, Type }!T {
    var ast = try std.zig.Ast.parse(allocator, source, .zon);
    defer ast.deinit(allocator);
    assert(ast.errors.len == 0);
    return parse(allocator, T, &ast, null);
}

pub fn parseFree(allocator: Allocator, value: anytype) void {
    switch (@typeInfo(@TypeOf(value))) {
        .Bool, .Int, .Float, .Enum => {},
        // TODO: chase pointer once that's implemented
        // TODO: free vs desotry?
        .Pointer => return allocator.free(value),
        else => unreachable,
    }
}

pub fn parseExpr(self: *Parser, comptime T: type, node: NodeIndex) error{ OutOfMemory, Type }!T {
    switch (@typeInfo(T)) {
        .Bool => return self.parseBool(node),
        .Int, .Float => return self.parseNumber(T, node),
        .Enum => return self.parseEnumLiteral(T, node),
        // TODO: combined for now for strings...
        .Pointer, .Array => return self.parsePointer(T, node),
        // TODO: keep in sync with parseFree
        else => unreachable,
    }
}

fn parsePointer(self: *Parser, comptime T: type, node: NodeIndex) error{ OutOfMemory, Type }!T {
    const tags = self.ast.nodes.items(.tag);
    return switch (tags[node]) {
        .string_literal => try self.parseStringLiteral(T, node),
        // XXX: support other slice syntaxes!
        else => return self.failExpectedType(T, node),
    };
}

// XXX: WIP:
// - to store a string literal in an array, we should need .*
//      + would be convenient to allow capped size strings to be stored inline
// - eventually write result to out parameter pointer instead of returning it? or does it not matter?
// - assuming string literal syntax, not supporting other types of arrays right now
// - 0 termination?
// - wide strings?
// - are single chars valid in place of string literals?
// - we have to handle invalid string literals here, the parser doesn't catch those errors in advance
//   right?
// - error handling ehre not really implemented yet, lots of debug mode assertions for now
fn parseStringLiteral(self: *Parser, comptime T: type, node: NodeIndex) error{ OutOfMemory, Type }!T {
    const tokens = self.ast.nodes.items(.main_token);
    const token = tokens[node];
    const raw_string = self.ast.tokenSlice(token);
    switch (@typeInfo(T)) {
        .Pointer => |Pointer| {
            comptime assert(Pointer.size == .Slice);
            comptime assert(Pointer.child == u8);
            return std.zig.string_literal.parseAlloc(self.allocator, raw_string) catch unreachable;
        },
        .Array => {
            var result: T = undefined;
            var fsw = std.io.fixedBufferStream(&result);
            _ = std.zig.string_literal.parseWrite(fsw.writer(), raw_string) catch unreachable;
            return result;
        },
        else => unreachable,
    }
}

test "string literal" {
    const allocator = std.testing.allocator;

    // Basic string literal
    {
        const parsed = try parseSlice(allocator, []const u8, "\"abc\"");
        defer parseFree(allocator, parsed);
        try std.testing.expectEqualSlices(u8, @as([]const u8, "abc"), parsed);
    }

    // String literal with escape characters
    {
        const parsed = try parseSlice(allocator, []const u8, "\"ab\\nc\"");
        defer parseFree(allocator, parsed);
        try std.testing.expectEqualSlices(u8, @as([]const u8, "ab\nc"), parsed);
    }

    // Fixed size string literal
    {
        const parsed = try parseSlice(allocator, [4]u8, "\"ab\\nc\"");
        try std.testing.expectEqualSlices(u8, "ab\nc", &parsed);
    }

    // XXX: implement these tests
    // // Too short
    // {
    //     const parsed = try parseSlice(allocator, [3]u8, "\"ab\"");
    //     try std.testing.expectEqualSlices(u8, "ab\nc", &parsed);
    // }

    // // Too long
    // {
    //     const parsed = try parseSlice(allocator, [3]u8, "\"abcd\"");
    //     try std.testing.expectEqualSlices(u8, "ab\nc", &parsed);
    // }

    // XXX: mutable vs non mutable (allowed?), freeing, limited size strings etc
    // XXX: allow references to literals or no?
}

// TODO: cannot represent not quite right error for unknown field right?
fn parseEnumLiteral(self: *Parser, comptime T: type, node: NodeIndex) error{Type}!T {
    const tags = self.ast.nodes.items(.tag);
    return switch (tags[node]) {
        .enum_literal => try self.parseEnumTag(T, node),
        .number_literal, .negation => try self.parseEnumNumber(T, node),
        else => return self.failExpectedType(T, node),
    };
}

fn parseEnumNumber(self: *Parser, comptime T: type, node: NodeIndex) error{Type}!T {
    // TODO: kinda weird dup error handling?
    var number = self.parseNumber(@typeInfo(T).Enum.tag_type, node) catch
        return self.failCannotRepresent(T, node);
    return std.meta.intToEnum(T, number) catch
        self.failCannotRepresent(T, node);
}

fn parseEnumTag(self: *Parser, comptime T: type, node: NodeIndex) error{Type}!T {
    // Create a comptime string map for the enum fields
    const enum_fields = @typeInfo(T).Enum.fields;
    comptime var kvs_list: [enum_fields.len]struct { []const u8, T } = undefined;
    inline for (enum_fields, 0..) |field, i| {
        kvs_list[i] = .{ field.name, @intToEnum(T, field.value) };
    }
    const tags = std.ComptimeStringMap(T, kvs_list);

    // TODO: could technically optimize getter for the case where it doesn't fail
    // TODO: the optimizer is smart enough to move the getters into the orelse case for these sorts
    // of things right?
    // Get the tag if it exists
    const tokens = self.ast.nodes.items(.main_token);
    const data = self.ast.nodes.items(.data);
    const token = tokens[node];
    const bytes = self.ast.tokenSlice(token);
    const dot_node = data[node].lhs;
    return tags.get(bytes) orelse
        self.failCannotRepresent(T, dot_node);
}

test "enum literals" {
    const allocator = std.testing.allocator;

    const Enum = enum {
        foo,
        bar,
        baz,
    };

    // Tags that exist
    try std.testing.expectEqual(Enum.foo, try parseSlice(allocator, Enum, ".foo"));
    try std.testing.expectEqual(Enum.bar, try parseSlice(allocator, Enum, ".bar"));
    try std.testing.expectEqual(Enum.baz, try parseSlice(allocator, Enum, ".baz"));

    // Bad tag
    {
        var ast = try std.zig.Ast.parse(allocator, ".qux", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, Enum, &ast, &err));
        try std.testing.expectEqualSlices(u8, err.cannot_represent.name, @typeName(Enum));
        const node = err.cannot_represent.node;
        const tokens = ast.nodes.items(.main_token);
        const token = tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 4,
        }, location);
    }

    // Bad type
    {
        var ast = try std.zig.Ast.parse(allocator, "true", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, Enum, &ast, &err));
        try std.testing.expectEqualSlices(u8, err.expected_type.name, @typeName(Enum));
        const node = err.expected_type.node;
        const tokens = ast.nodes.items(.main_token);
        const token = tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 4,
        }, location);
    }
}

test "numeric enum literals" {
    const allocator = std.testing.allocator;

    const Enum = enum(u8) {
        zero,
        three = 3,
    };

    const SignedEnum = enum(i8) {
        zero,
        three = -3,
    };

    // Test parsing numbers as enums
    try std.testing.expectEqual(Enum.zero, try parseSlice(allocator, Enum, "0"));
    try std.testing.expectEqual(Enum.three, try parseSlice(allocator, Enum, "3"));
    try std.testing.expectEqual(SignedEnum.zero, try parseSlice(allocator, SignedEnum, "0"));
    try std.testing.expectEqual(SignedEnum.three, try parseSlice(allocator, SignedEnum, "-3"));

    // Bad tag
    {
        var ast = try std.zig.Ast.parse(allocator, "2", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, Enum, &ast, &err));
        // XXX: use expect slice for these
        try std.testing.expectEqualSlices(u8, err.cannot_represent.name, @typeName(Enum));
        const node = err.cannot_represent.node;
        const tokens = ast.nodes.items(.main_token);
        const token = tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 1,
        }, location);
    }

    // Out of range tag
    {
        var ast = try std.zig.Ast.parse(allocator, "256", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, Enum, &ast, &err));
        try std.testing.expectEqualSlices(u8, err.cannot_represent.name, @typeName(Enum));
        const node = err.cannot_represent.node;
        const tokens = ast.nodes.items(.main_token);
        const token = tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 3,
        }, location);
    }

    // Unexpected negative tag
    {
        var ast = try std.zig.Ast.parse(allocator, "-3", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, Enum, &ast, &err));
        try std.testing.expectEqualSlices(u8, err.cannot_represent.name, @typeName(Enum));
        const node = err.cannot_represent.node;
        const tokens = ast.nodes.items(.main_token);
        const token = tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 2,
        }, location);
    }

    // Float tag
    {
        var ast = try std.zig.Ast.parse(allocator, "1.5", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, Enum, &ast, &err));
        try std.testing.expectEqualSlices(u8, err.cannot_represent.name, @typeName(Enum));
        const node = err.cannot_represent.node;
        const tokens = ast.nodes.items(.main_token);
        const token = tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 3,
        }, location);
    }

    // TODO: name general purpose allocators gpa!
}

// XXX: How do I free the results if failure occurs?
fn fail(self: *Parser, err: Error) error{Type} {
    @setCold(true);
    // XXX: ...commented out cause of dup error handling
    // assert(self.err == null);
    if (self.err) |e| {
        e.* = err;
    }
    return error.Type;
}

fn failExpectedType(self: *Parser, comptime T: type, node: NodeIndex) error{Type} {
    @setCold(true);
    return self.fail(.{ .expected_type = .{
        .name = @typeName(T),
        .node = node,
    } });
}

fn failCannotRepresent(self: *Parser, comptime T: type, node: NodeIndex) error{Type} {
    @setCold(true);
    return self.fail(.{ .cannot_represent = .{
        .name = @typeName(T),
        .node = node,
    } });
}

fn parseBool(self: *Parser, node: NodeIndex) error{Type}!bool {
    const tags = self.ast.nodes.items(.tag);
    const tokens = self.ast.nodes.items(.main_token);
    const token = tokens[node];
    switch (tags[node]) {
        .identifier => {
            const bytes = self.ast.tokenSlice(token);
            if (std.mem.eql(u8, bytes, "true")) {
                return true;
            } else if (std.mem.eql(u8, bytes, "false")) {
                return false;
            }
        },
        else => {},
    }
    return self.failExpectedType(bool, node);
}

test "parse bool" {
    const allocator = std.testing.allocator;

    // Correct floats
    try std.testing.expectEqual(true, try parseSlice(allocator, bool, "true"));
    try std.testing.expectEqual(false, try parseSlice(allocator, bool, "false"));

    // Errors
    {
        var ast = try std.zig.Ast.parse(allocator, " foo", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, bool, &ast, &err));
        try std.testing.expectEqualSlices(u8, err.expected_type.name, @typeName(bool));
        const node = err.expected_type.node;
        const tokens = ast.nodes.items(.main_token);
        const token = tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 1,
            .line_start = 0,
            .line_end = 4,
        }, location);
    }
    {
        var ast = try std.zig.Ast.parse(allocator, "123", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, bool, &ast, &err));
        try std.testing.expectEqualSlices(u8, err.expected_type.name, @typeName(bool));
        const node = err.expected_type.node;
        const tokens = ast.nodes.items(.main_token);
        const token = tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 3,
        }, location);
    }
}

// TODO: could set unused lhs/rhs to undefined?
fn parseNumber(
    self: *Parser,
    comptime T: type,
    node: NodeIndex,
) error{Type}!T {
    const num_lit_node = self.numLitNode(node);
    const tags = self.ast.nodes.items(.tag);
    switch (tags[num_lit_node]) {
        .number_literal => return self.parseNumberLiteral(T, node),
        .char_literal => return self.parseCharLiteral(T, node),
        else => return self.failExpectedType(T, num_lit_node),
    }
}

fn parseNumberLiteral(self: *Parser, comptime T: type, node: NodeIndex) error{Type}!T {
    const num_lit_node = self.numLitNode(node);
    const tokens = self.ast.nodes.items(.main_token);
    const num_lit_token = tokens[num_lit_node];
    const token_bytes = self.ast.tokenSlice(num_lit_token);
    const number = std.zig.number_literal.parseNumberLiteral(token_bytes);

    switch (number) {
        .int => |int| return self.applySignToInt(T, node, int),
        .big_int => |base| return self.parseBigNumber(T, node, base),
        .float => return self.parseFloat(T, node),
        else => unreachable,
    }
}

fn applySignToInt(self: *Parser, comptime T: type, node: NodeIndex, int: anytype) error{Type}!T {
    if (self.isNegative(node)) {
        switch (@typeInfo(T)) {
            .Int => |int_type| switch (int_type.signedness) {
                .signed => {
                    const Positive = @Type(.{ .Int = .{
                        .bits = int_type.bits + 1,
                        .signedness = .signed,
                    } });
                    if (int > std.math.maxInt(T) + 1) {
                        return self.failCannotRepresent(T, node);
                    }
                    const positive = @intCast(Positive, int);
                    return @intCast(T, -positive);
                },
                .unsigned => return self.failCannotRepresent(T, node),
            },
            .Float => return -@intToFloat(T, int),
            else => @compileError("expected numeric type"),
        }
    } else {
        switch (@typeInfo(T)) {
            .Int => return std.math.cast(T, int) orelse
                self.failCannotRepresent(T, node),
            .Float => return @intToFloat(T, int),
            else => @compileError("expected numeric type"),
        }
    }
}

fn parseBigNumber(
    self: *Parser,
    comptime T: type,
    node: NodeIndex,
    base: Base,
) error{Type}!T {
    switch (@typeInfo(T)) {
        .Int => return self.parseBigInt(T, node, base),
        // TODO: passing in f128 to work around possible float parsing bug
        .Float => return @floatCast(T, try self.parseFloat(f128, node)),
        else => unreachable,
    }
}

fn parseBigInt(self: *Parser, comptime T: type, node: NodeIndex, base: Base) error{Type}!T {
    const num_lit_node = self.numLitNode(node);
    const tokens = self.ast.nodes.items(.main_token);
    const num_lit_token = tokens[num_lit_node];
    // TODO: was wrong, passed in node by mistake! we could edit the ast to make this stuff typesafe..?
    const bytes = self.ast.tokenSlice(num_lit_token);
    const prefix_offset = @as(u8, 2) * @boolToInt(base != .decimal);
    var result: T = 0;
    for (bytes[prefix_offset..]) |char| {
        if (char == '_') continue;
        const d = std.fmt.charToDigit(char, @enumToInt(base)) catch unreachable;
        result = std.math.mul(T, result, @intCast(T, @enumToInt(base))) catch
            return self.failCannotRepresent(T, node);
        if (self.isNegative(node)) {
            result = std.math.sub(T, result, @intCast(T, d)) catch
                return self.failCannotRepresent(T, node);
        } else {
            result = std.math.add(T, result, @intCast(T, d)) catch
                return self.failCannotRepresent(T, node);
        }
    }
    return result;
}

fn parseFloat(
    self: *Parser,
    comptime T: type,
    node: NodeIndex,
) error{Type}!T {
    const Float = switch (@typeInfo(T)) {
        .Float => T,
        .Int => f128,
        else => unreachable,
    };
    const num_lit_node = self.numLitNode(node);
    const tokens = self.ast.nodes.items(.main_token);
    const num_lit_token = tokens[num_lit_node];
    const bytes = self.ast.tokenSlice(num_lit_token);
    const unsigned_float = std.fmt.parseFloat(Float, bytes) catch unreachable;
    const result = if (self.isNegative(node)) -unsigned_float else unsigned_float;
    if (T == Float) {
        return result;
    } else {
        return floatToInt(T, result) orelse
            self.failCannotRepresent(T, node);
    }
}

fn parseCharLiteral(self: *Parser, comptime T: type, node: NodeIndex) error{Type}!T {
    const num_lit_node = self.numLitNode(node);
    const tokens = self.ast.nodes.items(.main_token);
    const num_lit_token = tokens[num_lit_node];
    const token_bytes = self.ast.tokenSlice(num_lit_token);
    const char = std.zig.string_literal.parseCharLiteral(token_bytes).success;
    return self.applySignToInt(T, node, char);
}

// TODO: technically I can cache the results of this and numLitNode, but it's confusing. could maybe
// wrap in a struct that's like main node, negative node? or something.
fn isNegative(self: *const Parser, node: NodeIndex) bool {
    const tags = self.ast.nodes.items(.tag);
    return tags[node] == .negation;
}

fn numLitNode(self: *const Parser, node: NodeIndex) NodeIndex {
    if (self.isNegative(node)) {
        const data = self.ast.nodes.items(.data);
        return data[node].lhs;
    } else {
        return node;
    }
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
    try std.testing.expectEqual(@as(u8, 10), try parseSlice(allocator, u8, "10"));
    try std.testing.expectEqual(@as(i16, 24), try parseSlice(allocator, i16, "24"));
    try std.testing.expectEqual(@as(i14, -4), try parseSlice(allocator, i14, "-4"));
    try std.testing.expectEqual(@as(i32, -123), try parseSlice(allocator, i32, "-123"));

    // Test limits
    try std.testing.expectEqual(@as(i8, 127), try parseSlice(allocator, i8, "127"));
    try std.testing.expectEqual(@as(i8, -128), try parseSlice(allocator, i8, "-128"));

    // Test characters
    try std.testing.expectEqual(@as(u8, 'a'), try parseSlice(allocator, u8, "'a'"));
    try std.testing.expectEqual(@as(u8, 'z'), try parseSlice(allocator, u8, "'z'"));

    // Test big integers
    try std.testing.expectEqual(
        @as(u65, 36893488147419103231),
        try parseSlice(allocator, u65, "36893488147419103231"),
    );
    try std.testing.expectEqual(
        @as(u65, 36893488147419103231),
        try parseSlice(allocator, u65, "368934_881_474191032_31"),
    );

    // Test big integer limits
    try std.testing.expectEqual(
        @as(i66, 36893488147419103231),
        try parseSlice(allocator, i66, "36893488147419103231"),
    );
    try std.testing.expectEqual(
        @as(i66, -36893488147419103232),
        try parseSlice(allocator, i66, "-36893488147419103232"),
    );
    {
        var ast = try std.zig.Ast.parse(allocator, "36893488147419103232", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, i66, &ast, &err));
        try std.testing.expectEqualSlices(u8, err.cannot_represent.name, @typeName(i66));
        const node = err.cannot_represent.node;
        const tokens = ast.nodes.items(.main_token);
        const token = tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 20,
        }, location);
    }
    {
        var ast = try std.zig.Ast.parse(allocator, "-36893488147419103233", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, i66, &ast, &err));
        try std.testing.expectEqualSlices(u8, err.cannot_represent.name, @typeName(i66));
        const node = err.cannot_represent.node;
        const tokens = ast.nodes.items(.main_token);
        const token = tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 21,
        }, location);
    }

    // Test parsing whole number floats as integers
    try std.testing.expectEqual(@as(i8, -1), try parseSlice(allocator, i8, "-1.0"));
    try std.testing.expectEqual(@as(i8, 123), try parseSlice(allocator, i8, "123.0"));

    // Test non-decimal integers
    try std.testing.expectEqual(@as(i16, 0xff), try parseSlice(allocator, i16, "0xff"));
    try std.testing.expectEqual(@as(i16, -0xff), try parseSlice(allocator, i16, "-0xff"));
    try std.testing.expectEqual(@as(i16, 0o77), try parseSlice(allocator, i16, "0o77"));
    try std.testing.expectEqual(@as(i16, -0o77), try parseSlice(allocator, i16, "-0o77"));
    try std.testing.expectEqual(@as(i16, 0b11), try parseSlice(allocator, i16, "0b11"));
    try std.testing.expectEqual(@as(i16, -0b11), try parseSlice(allocator, i16, "-0b11"));

    // Test non-decimal big integers
    try std.testing.expectEqual(@as(u65, 0x1ffffffffffffffff), try parseSlice(
        allocator,
        u65,
        "0x1ffffffffffffffff",
    ));
    try std.testing.expectEqual(@as(i66, 0x1ffffffffffffffff), try parseSlice(
        allocator,
        i66,
        "0x1ffffffffffffffff",
    ));
    try std.testing.expectEqual(@as(i66, -0x1ffffffffffffffff), try parseSlice(
        allocator,
        i66,
        "-0x1ffffffffffffffff",
    ));
    try std.testing.expectEqual(@as(u65, 0x1ffffffffffffffff), try parseSlice(
        allocator,
        u65,
        "0o3777777777777777777777",
    ));
    try std.testing.expectEqual(@as(i66, 0x1ffffffffffffffff), try parseSlice(
        allocator,
        i66,
        "0o3777777777777777777777",
    ));
    try std.testing.expectEqual(@as(i66, -0x1ffffffffffffffff), try parseSlice(
        allocator,
        i66,
        "-0o3777777777777777777777",
    ));
    try std.testing.expectEqual(@as(u65, 0x1ffffffffffffffff), try parseSlice(
        allocator,
        u65,
        "0b11111111111111111111111111111111111111111111111111111111111111111",
    ));
    try std.testing.expectEqual(@as(i66, 0x1ffffffffffffffff), try parseSlice(
        allocator,
        i66,
        "0b11111111111111111111111111111111111111111111111111111111111111111",
    ));
    try std.testing.expectEqual(@as(i66, -0x1ffffffffffffffff), try parseSlice(
        allocator,
        i66,
        "-0b11111111111111111111111111111111111111111111111111111111111111111",
    ));

    // Failinig to parse as int
    {
        var ast = try std.zig.Ast.parse(allocator, "true", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, u8, &ast, &err));
        try std.testing.expectEqualSlices(u8, err.expected_type.name, @typeName(u8));
        const node = err.expected_type.node;
        const tokens = ast.nodes.items(.main_token);
        const token = tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 4,
        }, location);
    }

    // Failing because an int is out of range
    {
        var ast = try std.zig.Ast.parse(allocator, "256", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, u8, &ast, &err));
        try std.testing.expectEqualSlices(u8, err.cannot_represent.name, @typeName(u8));
        const node = err.cannot_represent.node;
        const tokens = ast.nodes.items(.main_token);
        const token = tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 3,
        }, location);
    }

    // Failing because a negative int is out of range
    {
        var ast = try std.zig.Ast.parse(allocator, "-129", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, i8, &ast, &err));
        try std.testing.expectEqualSlices(u8, err.cannot_represent.name, @typeName(i8));
        const node = err.cannot_represent.node;
        const tokens = ast.nodes.items(.main_token);
        const token = tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 4,
        }, location);
    }

    // Failing because an unsigned int is negative
    {
        var ast = try std.zig.Ast.parse(allocator, "-1", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, u8, &ast, &err));
        try std.testing.expectEqualSlices(u8, err.cannot_represent.name, @typeName(u8));
        const node = err.cannot_represent.node;
        const tokens = ast.nodes.items(.main_token);
        const token = tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 2,
        }, location);
    }

    // Failing because a float is non-whole
    {
        var ast = try std.zig.Ast.parse(allocator, "1.5", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, u8, &ast, &err));
        try std.testing.expectEqualSlices(u8, err.cannot_represent.name, @typeName(u8));
        const node = err.cannot_represent.node;
        const tokens = ast.nodes.items(.main_token);
        const token = tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 3,
        }, location);
    }

    // Failing because a float is negative
    {
        var ast = try std.zig.Ast.parse(allocator, "-1.0", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, u8, &ast, &err));
        try std.testing.expectEqualSlices(u8, err.cannot_represent.name, @typeName(u8));
        const node = err.cannot_represent.node;
        const tokens = ast.nodes.items(.main_token);
        const token = tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 4,
        }, location);
    }
}

test "parse float" {
    const allocator = std.testing.allocator;

    // Test decimals
    try std.testing.expectEqual(@as(f16, 0.5), try parseSlice(allocator, f16, "0.5"));
    try std.testing.expectEqual(@as(f32, 123.456), try parseSlice(allocator, f32, "123.456"));
    try std.testing.expectEqual(@as(f64, -123.456), try parseSlice(allocator, f64, "-123.456"));
    try std.testing.expectEqual(@as(f128, 42.5), try parseSlice(allocator, f128, "42.5"));

    // Test whole numbers with and without decimals
    try std.testing.expectEqual(@as(f16, 5.0), try parseSlice(allocator, f16, "5.0"));
    try std.testing.expectEqual(@as(f16, 5.0), try parseSlice(allocator, f16, "5"));
    try std.testing.expectEqual(@as(f32, -102), try parseSlice(allocator, f32, "-102.0"));
    try std.testing.expectEqual(@as(f32, -102), try parseSlice(allocator, f32, "-102"));

    // Test characters and negated characters
    try std.testing.expectEqual(@as(f32, 'a'), try parseSlice(allocator, f32, "'a'"));
    try std.testing.expectEqual(@as(f32, 'z'), try parseSlice(allocator, f32, "'z'"));
    try std.testing.expectEqual(@as(f32, -'z'), try parseSlice(allocator, f32, "-'z'"));

    // Test big integers
    try std.testing.expectEqual(
        @as(f32, 36893488147419103231),
        try parseSlice(allocator, f32, "36893488147419103231"),
    );
    try std.testing.expectEqual(
        @as(f32, -36893488147419103231),
        try parseSlice(allocator, f32, "-36893488147419103231"),
    );
    try std.testing.expectEqual(@as(f128, 0x1ffffffffffffffff), try parseSlice(
        allocator,
        f128,
        "0x1ffffffffffffffff",
    ));
    try std.testing.expectEqual(@as(f32, 0x1ffffffffffffffff), try parseSlice(
        allocator,
        f32,
        "0x1ffffffffffffffff",
    ));

    // Exponents, underscores
    try std.testing.expectEqual(@as(f32, 123.0E+77), try parseSlice(allocator, f32, "12_3.0E+77"));

    // Hexadecimal
    try std.testing.expectEqual(@as(f32, 0x103.70p-5), try parseSlice(allocator, f32, "0x103.70p-5"));
    try std.testing.expectEqual(@as(f32, -0x103.70), try parseSlice(allocator, f32, "-0x103.70"));
    try std.testing.expectEqual(
        @as(f32, 0x1234_5678.9ABC_CDEFp-10),
        try parseSlice(allocator, f32, "0x1234_5678.9ABC_CDEFp-10"),
    );
}

// TODO: zig float parsing bug example
// test "bug" {
//     const float: f32 = 0xffffffffffffffff;
//     const parsed = try std.fmt.parseFloat(f32, "0xffffffffffffffff.0p0");
//     try std.testing.expectEqual(float, parsed);
// }
