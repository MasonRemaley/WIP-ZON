const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = std.zig.Ast;
const NodeIndex = std.zig.Ast.Node.Index;
const TokenIndex = std.zig.Ast.TokenIndex;
const Type = std.builtin.Type;
const Base = std.zig.number_literal.Base;
const FloatBase = std.zig.number_literal.FloatBase;
const StringLiteralError = std.zig.string_literal.Error;
const assert = std.debug.assert;

const Parser = @This();

allocator: Allocator,
ast: *const Ast,
err: ?*Error,

// TODO: make a render errors function...handle underlines as well as point of error like zig? How?
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
    // TODO: doesn't point to specific character right now
    invalid_string_literal: struct {
        node: NodeIndex,
        reason: StringLiteralError,
    },
    unknown_field: struct {
        node: NodeIndex,
        type_name: []const u8,
        field_name: []const u8,
    },
    missing_field: struct {
        node: NodeIndex,
        type_name: []const u8,
        field_name: []const u8,
    },
};

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

test "error literals" {
    // TODO: can't return error!error, i think, so we need to use an out param, or not support this...
    // const allocator = std.testing.allocator;
    // const parsed = try parseSlice(allocator, anyerror, "error.Foo");
    // try std.testing.expectEqual(error.Foo, parsed);
}

pub fn parseSlice(allocator: Allocator, comptime T: type, source: [:0]const u8) error{ OutOfMemory, Type }!T {
    var ast = try std.zig.Ast.parse(allocator, source, .zon);
    defer ast.deinit(allocator);
    assert(ast.errors.len == 0);
    return parse(allocator, T, &ast, null);
}

pub fn parseFree(allocator: Allocator, value: anytype) void {
    const Value = @TypeOf(value);

    switch (@typeInfo(Value)) {
        .Bool, .Int, .Float, .Enum => {},
        .Pointer => |Pointer| {
            switch (Pointer.size) {
                .One => parseFree(allocator, value.*),
                // TODO: ...
                .Many, .C => @compileError("unsupported type"),
                // TODO: sentinels?
                .Slice => for (value) |item| {
                    parseFree(allocator, item);
                },
            }
            return allocator.free(value);
        },
        .Array => for (value) |item| {
            parseFree(allocator, item);
        },
        .Struct => |Struct| inline for (Struct.fields) |field| {
            parseFree(allocator, @field(value, field.name));
        },
        .Union => |Union| {
            const Tag = Union.tag_type orelse failFreeType(Value);
            inline for (@typeInfo(Tag).Enum.fields, 0..) |field, i| {
                const tag = @intToEnum(Tag, i);
                if (value == tag) {
                    parseFree(allocator, @field(value, field.name));
                    break;
                }
            }
        },
        .Optional => if (value) |some| {
            parseFree(allocator, some);
        },
        // TODO: ...
        else => failFreeType(Value),
    }
}

pub fn parseExpr(self: *Parser, comptime T: type, node: NodeIndex) error{ OutOfMemory, Type }!T {
    // TODO: keep in sync with parseFree
    switch (@typeInfo(T)) {
        // TODO: better errors for this?
        .Bool => return self.parseBool(node),
        .Int, .Float => return self.parseNumber(T, node),
        .Enum => return self.parseEnumLiteral(T, node),
        // TODO: combined for now for strings...
        .Pointer => return self.parsePointer(T, node),
        .Array => return self.parseArray(T, node),
        .Struct => |Struct| if (Struct.is_tuple)
            return self.parseTuple(T, node)
        else
            return self.parseStruct(T, node),
        .Union => return self.parseUnion(T, node),
        .Optional => return self.parseOptional(T, node),

        else => failToParseType(T),
    }
}

fn parseOptional(self: *Parser, comptime T: type, node: NodeIndex) error{ OutOfMemory, Type }!T {
    const Optional = @typeInfo(T).Optional;

    const tags = self.ast.nodes.items(.tag);
    if (tags[node] == .identifier) {
        const main_tokens = self.ast.nodes.items(.main_token);
        const token = main_tokens[node];
        const bytes = self.ast.tokenSlice(token);
        if (std.mem.eql(u8, bytes, "null")) {
            return null;
        }
    }

    return try self.parseExpr(Optional.child, node);
}

test "optional" {
    const allocator = std.testing.allocator;

    // Basic usage
    {
        const none = try parseSlice(allocator, ?u32, "null");
        try std.testing.expect(none == null);
        const some = try parseSlice(allocator, ?u32, "1");
        try std.testing.expect(some.? == 1);
    }

    // Deep free
    {
        const none = try parseSlice(allocator, ?[]const u8, "null");
        try std.testing.expect(none == null);
        const some = try parseSlice(allocator, ?[]const u8, "\"foo\"");
        defer parseFree(allocator, some);
        try std.testing.expectEqualStrings("foo", some.?);
    }
}

fn parseUnion(self: *Parser, comptime T: type, node: NodeIndex) error{ OutOfMemory, Type }!T {
    // TODO: some of the array errors point to the brace instead of 0?
    const Union = @typeInfo(T).Union;
    const field_infos = Union.fields;

    if (field_infos.len == 0) {
        failToParseType(T);
    }

    // Gather info on the fields
    const field_indices = b: {
        comptime var kvs_list: [field_infos.len]struct { []const u8, usize } = undefined;
        inline for (field_infos, 0..) |field, i| {
            kvs_list[i] = .{ field.name, i };
        }
        break :b std.ComptimeStringMap(usize, kvs_list);
    };

    // Parse the union
    var buf: [2]NodeIndex = undefined;
    const field_nodes = try self.elementsOrFields(T, &buf, node);

    if (field_nodes.len != 1) {
        return self.failExpectedType(T, node);
    }

    // Fill in the field we found
    const main_tokens = self.ast.nodes.items(.main_token);
    const field_node = field_nodes[0];
    const name = self.ast.tokenSlice(main_tokens[field_node] - 2);
    const i = field_indices.get(name) orelse
        return self.failUnknownField(T, field_node, name);

    inline for (0..field_infos.len) |j| {
        if (i == j) {
            const field_info = field_infos[j];
            const value = try self.parseExpr(field_info.type, field_node);
            return @unionInit(T, field_info.name, value);
        }
    }

    unreachable;
}

test "unions" {
    const allocator = std.testing.allocator;

    // Unions
    {
        const Tagged = union(enum) { x: f32, y: bool };
        const Untagged = union { x: f32, y: bool };

        const tagged_x = try parseSlice(allocator, Tagged, ".{.x = 1.5}");
        try std.testing.expectEqual(Tagged{ .x = 1.5 }, tagged_x);
        const tagged_y = try parseSlice(allocator, Tagged, ".{.y = true}");
        try std.testing.expectEqual(Tagged{ .y = true }, tagged_y);

        const untagged_x = try parseSlice(allocator, Untagged, ".{.x = 1.5}");
        try std.testing.expect(untagged_x.x == 1.5);
        const untagged_y = try parseSlice(allocator, Untagged, ".{.y = true}");
        try std.testing.expect(untagged_y.y);
    }

    // Deep free
    {
        const Union = union(enum) { bar: []const u8, baz: bool };

        const noalloc = try parseSlice(allocator, Union, ".{.baz = false}");
        try std.testing.expectEqual(Union{ .baz = false }, noalloc);

        const alloc = try parseSlice(allocator, Union, ".{.bar = \"qux\"}");
        defer parseFree(allocator, alloc);
        try std.testing.expectEqualDeep(Union{ .bar = "qux" }, alloc);
    }

    // Unknown field
    {
        const Union = struct { x: f32, y: f32 };
        var ast = try std.zig.Ast.parse(allocator, ".{.z=2.5}", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, Union, &ast, &err));
        try std.testing.expectEqualStrings(@typeName(Union), err.unknown_field.type_name);
        try std.testing.expectEqualStrings("z", err.unknown_field.field_name);
        const node = err.unknown_field.node;
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node] - 2;
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 3,
            .line_start = 0,
            .line_end = 9,
        }, location);
    }

    // Extra field
    {
        const Union = union { x: f32, y: bool };
        var ast = try std.zig.Ast.parse(allocator, ".{.x = 1.5, .y = true}", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, Union, &ast, &err));
        try std.testing.expectEqualStrings(@typeName(Union), err.expected_type.name);
        const node = err.expected_type.node;
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            // TODO: why column 1?
            .column = 1,
            .line_start = 0,
            .line_end = 22,
        }, location);
    }

    // No fields
    {
        const Union = union { x: f32, y: bool };
        var ast = try std.zig.Ast.parse(allocator, ".{}", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, Union, &ast, &err));
        try std.testing.expectEqualStrings(@typeName(Union), err.expected_type.name);
        const node = err.expected_type.node;
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            // TODO: why column 1?
            .column = 1,
            .line_start = 0,
            .line_end = 3,
        }, location);
    }
}

// TODO: modify the parser instead of using this workaround? (is necessary because arrays of size
// 0 are treated as structs)
// TODO: doesn't make error handling weird right?
fn elementsOrFields(
    self: *Parser,
    comptime T: type,
    buf: *[2]NodeIndex,
    node: NodeIndex,
) error{Type}![]const NodeIndex {
    if (self.ast.fullStructInit(buf, node)) |init| {
        return init.ast.fields;
    } else if (self.ast.fullArrayInit(buf, node)) |init| {
        return init.ast.elements;
    } else {
        return self.failExpectedType(T, node);
    }
}

// TODO: can bench with and without comptime string map later?
fn parseStruct(self: *Parser, comptime T: type, node: NodeIndex) error{ OutOfMemory, Type }!T {
    // TODO: some of the array errors point to the brace instead of 0?
    const Struct = @typeInfo(T).Struct;
    const main_tokens = self.ast.nodes.items(.main_token);
    const field_infos = Struct.fields;

    var result: T = undefined;

    if (field_infos.len > 0) {
        // Gather info on the fields
        const field_indices = b: {
            comptime var kvs_list: [field_infos.len]struct { []const u8, usize } = undefined;
            inline for (field_infos, 0..) |field, i| {
                kvs_list[i] = .{ field.name, i };
            }
            break :b std.ComptimeStringMap(usize, kvs_list);
        };

        // Parse the struct
        var buf: [2]NodeIndex = undefined;
        const field_nodes = try self.elementsOrFields(T, &buf, node);

        // Fill in the fields we found
        var field_found: [field_infos.len]bool = .{false} ** field_infos.len;
        for (field_nodes) |field_node| {
            const name = self.ast.tokenSlice(main_tokens[field_node] - 2);
            const i = field_indices.get(name) orelse
                return self.failUnknownField(T, field_node, name);

            inline for (0..field_infos.len) |j| {
                if (i == j) {
                    const field_info = field_infos[j];
                    @field(result, field_info.name) = try self.parseExpr(field_info.type, field_node);
                    break;
                }
            }

            field_found[i] = true;
        }

        // Fill in any missing default fields
        inline for (field_found, 0..) |found, i| {
            if (!found) {
                const field_info = Struct.fields[i];
                if (field_info.default_value) |default| {
                    const aligned = @alignCast(@alignOf(field_info.type), default);
                    const typed = @ptrCast(*const field_info.type, aligned);
                    @field(result, field_info.name) = typed.*;
                } else {
                    return self.failMissingField(T, field_infos[i].name, node);
                }
            }
        }
    }

    return result;
}

// TODO: should we be naming tests prefixed with zon?
test "structs" {
    const allocator = std.testing.allocator;

    // Structs (various sizes tested since they're parsed differently)
    {
        const Vec0 = struct {};
        const Vec1 = struct { x: f32 };
        const Vec2 = struct { x: f32, y: f32 };
        const Vec3 = struct { x: f32, y: f32, z: f32 };

        const zero = try parseSlice(allocator, Vec0, ".{}");
        try std.testing.expectEqual(Vec0{}, zero);

        const one = try parseSlice(allocator, Vec1, ".{.x = 1.2}");
        try std.testing.expectEqual(Vec1{ .x = 1.2 }, one);

        const two = try parseSlice(allocator, Vec2, ".{.x = 1.2, .y = 3.4}");
        try std.testing.expectEqual(Vec2{ .x = 1.2, .y = 3.4 }, two);

        const three = try parseSlice(allocator, Vec3, ".{.x = 1.2, .y = 3.4, .z = 5.6}");
        try std.testing.expectEqual(Vec3{ .x = 1.2, .y = 3.4, .z = 5.6 }, three);
    }

    // Deep free (structs and arrays)
    {
        const Foo = struct { bar: []const u8, baz: []const []const u8 };

        const parsed = try parseSlice(allocator, Foo, ".{.bar = \"qux\", .baz = &.{\"a\", \"b\"}}");
        defer parseFree(allocator, parsed);
        try std.testing.expectEqualDeep(Foo{ .bar = "qux", .baz = &.{ "a", "b" } }, parsed);
    }

    // Unknown field
    {
        const Vec2 = struct { x: f32, y: f32 };
        var ast = try std.zig.Ast.parse(allocator, ".{.x=1.5, .z=2.5}", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, Vec2, &ast, &err));
        try std.testing.expectEqualStrings(@typeName(Vec2), err.unknown_field.type_name);
        try std.testing.expectEqualStrings("z", err.unknown_field.field_name);
        const node = err.unknown_field.node;
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node] - 2;
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 11,
            .line_start = 0,
            .line_end = 17,
        }, location);
    }

    // Missing field
    {
        const Vec2 = struct { x: f32, y: f32 };
        var ast = try std.zig.Ast.parse(allocator, ".{.x=1.5}", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, Vec2, &ast, &err));
        try std.testing.expectEqualStrings(@typeName(Vec2), err.missing_field.type_name);
        try std.testing.expectEqualStrings("y", err.missing_field.field_name);
        const node = err.missing_field.node;
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            // TODO: why not zero?
            .column = 1,
            .line_start = 0,
            .line_end = 9,
        }, location);
    }

    // Default field
    {
        const Vec2 = struct { x: f32, y: f32 = 1.5 };
        const parsed = try parseSlice(allocator, Vec2, ".{.x = 1.2}");
        try std.testing.expectEqual(Vec2{ .x = 1.2, .y = 1.5 }, parsed);
    }
}

fn parseTuple(self: *Parser, comptime T: type, node: NodeIndex) error{ OutOfMemory, Type }!T {
    const Struct = @typeInfo(T).Struct;
    const field_infos = Struct.fields;

    var result: T = undefined;

    // Parse the struct
    var buf: [2]NodeIndex = undefined;
    const field_nodes = try self.elementsOrFields(T, &buf, node);

    if (field_nodes.len != field_infos.len) {
        // TODO: is this similar to the error zig gives?
        return self.failExpectedType(T, node);
    }

    inline for (field_infos, field_nodes, 0..) |field_info, field_node, i| {
        result[i] = try self.parseExpr(field_info.type, field_node);
    }

    return result;
}

test "tuples" {
    const allocator = std.testing.allocator;

    // Structs (various sizes tested since they're parsed differently)
    {
        const Tuple0 = struct {};
        const Tuple1 = struct { f32 };
        const Tuple2 = struct { f32, bool };
        const Tuple3 = struct { f32, bool, u8 };

        const zero = try parseSlice(allocator, Tuple0, ".{}");
        try std.testing.expectEqual(Tuple0{}, zero);

        const one = try parseSlice(allocator, Tuple1, ".{1.2}");
        try std.testing.expectEqual(Tuple1{1.2}, one);

        const two = try parseSlice(allocator, Tuple2, ".{1.2, true}");
        try std.testing.expectEqual(Tuple2{ 1.2, true }, two);

        const three = try parseSlice(allocator, Tuple3, ".{1.2, false, 3}");
        try std.testing.expectEqual(Tuple3{ 1.2, false, 3 }, three);
    }

    // Deep free
    {
        const Tuple = struct { []const u8, []const u8 };
        const parsed = try parseSlice(allocator, Tuple, ".{\"hello\", \"world\"}");
        defer parseFree(allocator, parsed);
        try std.testing.expectEqualDeep(Tuple{ "hello", "world" }, parsed);
    }

    // Extra field
    {
        const Tuple = struct { f32, bool };
        var ast = try std.zig.Ast.parse(allocator, ".{0.5, true, 123}", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, Tuple, &ast, &err));
        try std.testing.expectEqualStrings(@typeName(Tuple), err.expected_type.name);
        const node = err.expected_type.node;
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            // TODO: why column 1?
            .column = 1,
            .line_start = 0,
            .line_end = 17,
        }, location);
    }

    // Extra field
    {
        const Tuple = struct { f32, bool };
        var ast = try std.zig.Ast.parse(allocator, ".{0.5}", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, Tuple, &ast, &err));
        try std.testing.expectEqualStrings(@typeName(Tuple), err.expected_type.name);
        const node = err.expected_type.node;
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            // TODO: why column 1?
            .column = 1,
            .line_start = 0,
            .line_end = 6,
        }, location);
    }
}

fn parseArray(self: *Parser, comptime T: type, node: NodeIndex) error{ OutOfMemory, Type }!T {
    const Array = @typeInfo(T).Array;
    // TODO: passing in a buffer is a reasonable pattern for this kinda thing, could use elsewhere?
    // TODO: why .ast?
    // Parse the array
    var array: T = undefined;
    var buf: [2]NodeIndex = undefined;
    const element_nodes = try self.elementsOrFields(T, &buf, node);

    // Check if the size matches
    if (element_nodes.len != Array.len) {
        return self.failExpectedType(T, node);
    }

    // Parse the elements and return the array
    for (&array, element_nodes) |*element, element_node| {
        element.* = try self.parseExpr(Array.child, element_node);
    }
    return array;
}

// Test sizes 0 to 3 since small sizes get parsed differently
test "arrays and slices" {
    const allocator = std.testing.allocator;

    // Literals
    {
        // Arrays
        {
            const zero = try parseSlice(allocator, [0]u8, ".{}");
            try std.testing.expectEqualSlices(u8, &@as([0]u8, .{}), &zero);

            const one = try parseSlice(allocator, [1]u8, ".{'a'}");
            try std.testing.expectEqualSlices(u8, &@as([1]u8, .{'a'}), &one);

            const two = try parseSlice(allocator, [2]u8, ".{'a', 'b'}");
            try std.testing.expectEqualSlices(u8, &@as([2]u8, .{ 'a', 'b' }), &two);

            const two_comma = try parseSlice(allocator, [2]u8, ".{'a', 'b',}");
            try std.testing.expectEqualSlices(u8, &@as([2]u8, .{ 'a', 'b' }), &two_comma);

            const three = try parseSlice(allocator, [3]u8, ".{'a', 'b', 'c'}");
            try std.testing.expectEqualSlices(u8, &.{ 'a', 'b', 'c' }, &three);

            const sentinel = try parseSlice(allocator, [3:'z']u8, ".{'a', 'b', 'c'}");
            const expected_sentinel: [3:'z']u8 = .{ 'a', 'b', 'c' };
            try std.testing.expectEqualSlices(u8, &expected_sentinel, &sentinel);
        }

        // Slice literals
        {
            const zero = try parseSlice(allocator, []const u8, "&.{}");
            defer parseFree(allocator, zero);
            try std.testing.expectEqualSlices(u8, @as([]const u8, &.{}), zero);

            const one = try parseSlice(allocator, []u8, "&.{'a'}");
            defer parseFree(allocator, one);
            try std.testing.expectEqualSlices(u8, &.{'a'}, one);

            const two = try parseSlice(allocator, []const u8, "&.{'a', 'b'}");
            defer parseFree(allocator, two);
            try std.testing.expectEqualSlices(u8, &.{ 'a', 'b' }, two);

            const two_comma = try parseSlice(allocator, []const u8, "&.{'a', 'b',}");
            defer parseFree(allocator, two_comma);
            try std.testing.expectEqualSlices(u8, &.{ 'a', 'b' }, two_comma);

            const three = try parseSlice(allocator, []u8, "&.{'a', 'b', 'c'}");
            defer parseFree(allocator, three);
            try std.testing.expectEqualSlices(u8, &.{ 'a', 'b', 'c' }, three);

            const sentinel = try parseSlice(allocator, [:'z']const u8, "&.{'a', 'b', 'c'}");
            defer parseFree(allocator, sentinel);
            const expected_sentinel: [:'z']const u8 = &.{ 'a', 'b', 'c' };
            try std.testing.expectEqualSlices(u8, expected_sentinel, sentinel);
        }
    }

    // Deep free
    {
        // Arrays
        {
            const parsed = try parseSlice(allocator, [1][]const u8, ".{\"abc\"}");
            defer parseFree(allocator, parsed);
            const expected: [1][]const u8 = .{"abc"};
            try std.testing.expectEqualDeep(expected, parsed);
        }

        // Slice literals
        {
            const parsed = try parseSlice(allocator, []const []const u8, "&.{\"abc\"}");
            defer parseFree(allocator, parsed);
            const expected: []const []const u8 = &.{"abc"};
            try std.testing.expectEqualDeep(expected, parsed);
        }
    }

    // Senintels and alignment
    {
        // Arrays
        {
            const sentinel = try parseSlice(allocator, [1:2]u8, ".{1}");
            try std.testing.expectEqual(@as(usize, 1), sentinel.len);
            try std.testing.expectEqual(@as(u8, 1), sentinel[0]);
            try std.testing.expectEqual(@as(u8, 2), sentinel[1]);
        }

        // Slice literals
        {
            const sentinel = try parseSlice(allocator, [:2]align(4) u8, "&.{1}");
            defer parseFree(allocator, sentinel);
            try std.testing.expectEqual(@as(usize, 1), sentinel.len);
            try std.testing.expectEqual(@as(u8, 1), sentinel[0]);
            try std.testing.expectEqual(@as(u8, 2), sentinel[1]);
        }
    }

    // Expect 0 find 3
    {
        var ast = try std.zig.Ast.parse(allocator, ".{'a', 'b', 'c'}", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, [0]u8, &ast, &err));
        try std.testing.expectEqualStrings(@typeName([0]u8), err.expected_type.name);
        const node = err.expected_type.node;
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 1,
            .line_start = 0,
            .line_end = 16,
        }, location);
    }

    // Expect 1 find 2
    {
        var ast = try std.zig.Ast.parse(allocator, ".{'a', 'b'}", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, [1]u8, &ast, &err));
        try std.testing.expectEqualStrings(@typeName([1]u8), err.expected_type.name);
        const node = err.expected_type.node;
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 1,
            .line_start = 0,
            .line_end = 11,
        }, location);
    }

    // Expect 2 find 1
    {
        var ast = try std.zig.Ast.parse(allocator, ".{'a'}", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, [2]u8, &ast, &err));
        try std.testing.expectEqualStrings(@typeName([2]u8), err.expected_type.name);
        const node = err.expected_type.node;
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 1,
            .line_start = 0,
            .line_end = 6,
        }, location);
    }

    // Expect 3 find 0
    {
        var ast = try std.zig.Ast.parse(allocator, ".{}", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, [3]u8, &ast, &err));
        try std.testing.expectEqualStrings(@typeName([3]u8), err.expected_type.name);
        const node = err.expected_type.node;
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 1,
            .line_start = 0,
            .line_end = 3,
        }, location);
    }

    // Wrong inner type
    {
        // Array
        {
            var ast = try std.zig.Ast.parse(allocator, ".{'a', 'b', 'c'}", .zon);
            defer ast.deinit(allocator);
            var err: Error = .success;
            try std.testing.expectError(error.Type, parse(allocator, [3]bool, &ast, &err));
            try std.testing.expectEqualStrings(@typeName(bool), err.expected_type.name);
            const node = err.expected_type.node;
            const main_tokens = ast.nodes.items(.main_token);
            const token = main_tokens[node];
            const location = ast.tokenLocation(0, token);
            try std.testing.expectEqual(Ast.Location{
                .line = 0,
                .column = 2,
                .line_start = 0,
                .line_end = 16,
            }, location);
        }

        // Slice
        {
            var ast = try std.zig.Ast.parse(allocator, "&.{'a', 'b', 'c'}", .zon);
            defer ast.deinit(allocator);
            var err: Error = .success;
            try std.testing.expectError(error.Type, parse(allocator, []bool, &ast, &err));
            try std.testing.expectEqualStrings(@typeName(bool), err.expected_type.name);
            const node = err.expected_type.node;
            const main_tokens = ast.nodes.items(.main_token);
            const token = main_tokens[node];
            const location = ast.tokenLocation(0, token);
            try std.testing.expectEqual(Ast.Location{
                .line = 0,
                .column = 3,
                .line_start = 0,
                .line_end = 17,
            }, location);
        }
    }

    // Complete wrong type
    {
        // Array
        {
            var ast = try std.zig.Ast.parse(allocator, "'a'", .zon);
            defer ast.deinit(allocator);
            var err: Error = .success;
            try std.testing.expectError(error.Type, parse(allocator, [3]u8, &ast, &err));
            try std.testing.expectEqualStrings(@typeName([3]u8), err.expected_type.name);
            const node = err.expected_type.node;
            const main_tokens = ast.nodes.items(.main_token);
            const token = main_tokens[node];
            const location = ast.tokenLocation(0, token);
            try std.testing.expectEqual(Ast.Location{
                .line = 0,
                .column = 0,
                .line_start = 0,
                .line_end = 3,
            }, location);
        }

        // Slice
        {
            var ast = try std.zig.Ast.parse(allocator, "'a'", .zon);
            defer ast.deinit(allocator);
            var err: Error = .success;
            try std.testing.expectError(error.Type, parse(allocator, []u8, &ast, &err));
            try std.testing.expectEqualStrings(@typeName([]u8), err.expected_type.name);
            const node = err.expected_type.node;
            const main_tokens = ast.nodes.items(.main_token);
            const token = main_tokens[node];
            const location = ast.tokenLocation(0, token);
            try std.testing.expectEqual(Ast.Location{
                .line = 0,
                .column = 0,
                .line_start = 0,
                .line_end = 3,
            }, location);
        }
    }

    // Mixing up arrays and slices
    {
        // Array
        {
            var ast = try std.zig.Ast.parse(allocator, "&.{'a', 'b', 'c'}", .zon);
            defer ast.deinit(allocator);
            var err: Error = .success;
            try std.testing.expectError(error.Type, parse(allocator, [3]bool, &ast, &err));
            try std.testing.expectEqualStrings(@typeName([3]bool), err.expected_type.name);
            const node = err.expected_type.node;
            const main_tokens = ast.nodes.items(.main_token);
            const token = main_tokens[node];
            const location = ast.tokenLocation(0, token);
            try std.testing.expectEqual(Ast.Location{
                .line = 0,
                .column = 0,
                .line_start = 0,
                .line_end = 17,
            }, location);
        }

        // Slice
        {
            var ast = try std.zig.Ast.parse(allocator, ".{'a', 'b', 'c'}", .zon);
            defer ast.deinit(allocator);
            var err: Error = .success;
            try std.testing.expectError(error.Type, parse(allocator, []bool, &ast, &err));
            try std.testing.expectEqualStrings(@typeName([]bool), err.expected_type.name);
            const node = err.expected_type.node;
            const main_tokens = ast.nodes.items(.main_token);
            const token = main_tokens[node];
            const location = ast.tokenLocation(0, token);
            try std.testing.expectEqual(Ast.Location{
                .line = 0,
                .column = 1,
                .line_start = 0,
                .line_end = 16,
            }, location);
        }
    }
}

fn parsePointer(self: *Parser, comptime T: type, node: NodeIndex) error{ OutOfMemory, Type }!T {
    const tags = self.ast.nodes.items(.tag);
    const data = self.ast.nodes.items(.data);
    return switch (tags[node]) {
        .string_literal => try self.parseStringLiteral(T, node),
        .address_of => try self.parseAddressOf(T, data[node].lhs),
        else => self.failExpectedType(T, node),
    };
}

fn parseAddressOf(self: *Parser, comptime T: type, node: NodeIndex) error{ OutOfMemory, Type }!T {
    const Ptr = @typeInfo(T).Pointer;
    // TODO: it may make sense to support coercing into these even though it won't often be used, since
    // zig does, so it's consistent. Not gonna bother for now though can revisit later and decide.
    // Make sure we're working with a slice
    switch (Ptr.size) {
        .One, .Many, .C => failToParseType(T),
        .Slice => {},
    }

    // Parse the array literal
    var buf: [2]NodeIndex = undefined;
    const element_nodes = try self.elementsOrFields(T, &buf, node);

    // Allocate the slice
    const sentinel = if (Ptr.sentinel) |s| @ptrCast(*const Ptr.child, s).* else null;
    var slice = try self.allocator.allocWithOptions(
        Ptr.child,
        element_nodes.len,
        Ptr.alignment,
        sentinel,
    );
    errdefer self.allocator.free(slice);

    // Parse the elements and return the slice
    for (slice, element_nodes) |*element, element_node| {
        element.* = try self.parseExpr(Ptr.child, element_node);
    }
    return slice;
}

fn parseStringLiteral(self: *Parser, comptime T: type, node: NodeIndex) !T {
    switch (@typeInfo(T)) {
        .Pointer => |Pointer| {
            if (Pointer.size != .Slice) {
                failToParseType(T);
            }

            const main_tokens = self.ast.nodes.items(.main_token);
            const token = main_tokens[node];
            const raw = self.ast.tokenSlice(token);

            if (Pointer.child != u8 or !Pointer.is_const or Pointer.alignment != 1) {
                return self.failExpectedType(T, node);
            }
            var buf = std.ArrayListUnmanaged(u8){};
            defer buf.deinit(self.allocator);
            switch (try std.zig.string_literal.parseWrite(buf.writer(self.allocator), raw)) {
                .success => {},
                .failure => |reason| return self.failInvalidStringLiteral(node, reason),
            }

            if (Pointer.sentinel) |sentinel| {
                if (@ptrCast(*const u8, sentinel).* != 0) {
                    return self.failExpectedType(T, node);
                }

                // TODO: why couldn't I use from owned slice for this before when it was getting converted
                // back and forth?
                // var temp = std.ArrayListUnmanaged(u8).fromOwnedSlice(result);
                // errdefer temp.deinit(self.allocator);
                // try temp.append(self.allocator, 0);
                // return temp.items[0 .. temp.items.len - 1 :0];

                try buf.append(self.allocator, 0);
                // TODO: doesn't alloc right?
                const result = try buf.toOwnedSlice(self.allocator);
                return result[0 .. result.len - 1 :0];
            }

            return try buf.toOwnedSlice(self.allocator);
        },
        .Array => |Array| {
            if (Array.sentinel) |sentinel| {
                if (@ptrCast(*const u8, sentinel).* != 0) {
                    return self.failExpectedType(T, node);
                }
            }

            if (Array.child != u8) {
                return self.failExpectedType(T, node);
            }

            const data = self.ast.nodes.items(.data);
            const literal = data[node].lhs;
            const main_tokens = self.ast.nodes.items(.main_token);
            const token = main_tokens[literal];
            const raw = self.ast.tokenSlice(token);

            // TODO: are undefined zero terminated arrays still terminated?
            var result: T = undefined;
            var fsw = std.io.fixedBufferStream(&result);
            const status = std.zig.string_literal.parseWrite(fsw.writer(), raw) catch |e| switch (e) {
                error.NoSpaceLeft => return self.failExpectedType(T, node),
            };
            switch (status) {
                .success => {},
                .failure => |reason| return self.failInvalidStringLiteral(literal, reason),
            }
            if (Array.len != fsw.pos) {
                return self.failExpectedType(T, node);
            }

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

    // Passing string literal to a mutable slice
    {
        var ast = try std.zig.Ast.parse(allocator, "\"abcd\"", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, []u8, &ast, &err));
        try std.testing.expectEqualStrings(@typeName([]u8), err.expected_type.name);
        const node = err.expected_type.node;
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 6,
        }, location);
    }

    // Zero termianted slices
    {
        const parsed: [:0]const u8 = try parseSlice(allocator, [:0]const u8, "\"abc\"");
        defer parseFree(allocator, parsed);
        try std.testing.expectEqualSlices(u8, "abc", parsed);
        try std.testing.expectEqual(@as(u8, 0), parsed[3]);
    }

    // Other value terminated slices
    {
        var ast = try std.zig.Ast.parse(allocator, "\"foo\"", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, [:1]const u8, &ast, &err));
        try std.testing.expectEqualStrings(@typeName([:1]const u8), err.expected_type.name);
        const node = err.expected_type.node;
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 5,
        }, location);
    }

    // Invalid string literal
    {
        var ast = try std.zig.Ast.parse(allocator, "\"\\a\"", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, []const u8, &ast, &err));
        const node = err.invalid_string_literal.node;
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 4,
        }, location);
    }

    // Slice wrong child type
    {
        var ast = try std.zig.Ast.parse(allocator, "\"a\"", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, []const i8, &ast, &err));
        try std.testing.expectEqualStrings(@typeName([]const i8), err.expected_type.name);
        const node = err.expected_type.node;
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 3,
        }, location);
    }

    // Bad alignment
    {
        var ast = try std.zig.Ast.parse(allocator, "\"abc\"", .zon);
        defer ast.deinit(allocator);
        var err: Error = .success;
        try std.testing.expectError(error.Type, parse(allocator, []align(2) const u8, &ast, &err));
        try std.testing.expectEqualStrings(@typeName([]align(2) const u8), err.expected_type.name);
        const node = err.expected_type.node;
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
        const location = ast.tokenLocation(0, token);
        try std.testing.expectEqual(Ast.Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 5,
        }, location);
    }

    // TODO: ...
    // // Multi line strins
    // {
    //     const parsed = try parseSlice(allocator, []const u8, "\\foo\\bar");
    //     defer parseFree(allocator, parsed);
    //     try std.testing.expectEqualSlices(u8, "foo\nbar", parsed);
    // }
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
    const main_tokens = self.ast.nodes.items(.main_token);
    const data = self.ast.nodes.items(.data);
    const token = main_tokens[node];
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
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
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
        try std.testing.expectEqualStrings(@typeName(Enum), err.expected_type.name);
        const node = err.expected_type.node;
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
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
        try std.testing.expectEqualSlices(u8, err.cannot_represent.name, @typeName(Enum));
        const node = err.cannot_represent.node;
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
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
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
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
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
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
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
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

// TODO: How do I free the results if failure occurs?
fn fail(self: *Parser, err: Error) error{Type} {
    @setCold(true);
    // TODO: ...commented out cause of dup error handling
    // assert(self.err == null);
    if (self.err) |e| {
        e.* = err;
    }
    return error.Type;
}

fn failInvalidStringLiteral(self: *Parser, node: NodeIndex, reason: StringLiteralError) error{Type} {
    @setCold(true);
    return self.fail(.{ .invalid_string_literal = .{
        .node = node,
        .reason = reason,
    } });
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

fn failUnknownField(self: *Parser, comptime T: type, node: NodeIndex, name: []const u8) error{Type} {
    // TODO: should we be using setCold anywhere else?
    @setCold(true);
    return self.fail(.{ .unknown_field = .{
        .node = node,
        .type_name = @typeName(T),
        .field_name = name,
    } });
}

fn failMissingField(self: *Parser, comptime T: type, name: []const u8, node: NodeIndex) error{Type} {
    @setCold(true);
    return self.fail(.{ .missing_field = .{
        .node = node,
        .type_name = @typeName(T),
        .field_name = name,
    } });
}

fn failToParseType(comptime T: type) noreturn {
    @compileError("unable to parse into type " ++ @typeName(T));
}

fn failFreeType(comptime T: type) noreturn {
    @compileError("unable to free type " ++ @typeName(T));
}

fn parseBool(self: *Parser, node: NodeIndex) error{Type}!bool {
    const tags = self.ast.nodes.items(.tag);
    const main_tokens = self.ast.nodes.items(.main_token);
    const token = main_tokens[node];
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
        try std.testing.expectEqualStrings(@typeName(bool), err.expected_type.name);
        const node = err.expected_type.node;
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
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
        try std.testing.expectEqualStrings(@typeName(bool), err.expected_type.name);
        const node = err.expected_type.node;
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
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
    const main_tokens = self.ast.nodes.items(.main_token);
    const num_lit_token = main_tokens[num_lit_node];
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
    const main_tokens = self.ast.nodes.items(.main_token);
    const num_lit_token = main_tokens[num_lit_node];
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
    const main_tokens = self.ast.nodes.items(.main_token);
    const num_lit_token = main_tokens[num_lit_node];
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
    const main_tokens = self.ast.nodes.items(.main_token);
    const num_lit_token = main_tokens[num_lit_node];
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
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
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
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
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
        try std.testing.expectEqualStrings(@typeName(u8), err.expected_type.name);
        const node = err.expected_type.node;
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
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
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
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
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
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
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
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
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
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
        const main_tokens = ast.nodes.items(.main_token);
        const token = main_tokens[node];
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
