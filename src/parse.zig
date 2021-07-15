const std = @import("std");
const testing = std.testing;

const lex = @import("lex.zig");

const assert_one_match = true;

const LexResult = struct {
    kind: lex.TokenKind,
    end: [*]const u8,
};
fn scan(text: [*]const u8, limit: [*]const u8) !LexResult {
    std.debug.assert(@ptrToInt(text) < @ptrToInt(limit));
    var kind = @intToEnum(lex.TokenKind, 0);
    const end = lex.lex(text, limit, &kind);
    if (end == text)
        return error.UnknownToken;
    return LexResult { .kind = kind, .end = end };
}
//pub const ScanSkipResult = struct {
//    start: [*]const u8,
//    scan_result: LexResult,
//};
//pub fn scanSkipInlineWhitespace(text: [*]const u8, limit: [*]const u8) !ScanSkipResult {
//    const result1 = try scan(text, limit);
//    std.debug.assert(@ptrToInt(result1.end) > @ptrToInt(text));
//    if (result1.kind != .inline_whitespace)
//        return .{ .start = text, .scan_result = result1 };
//    // BUG POTENTIAL!!! Make sure that when I scan src after an UnknownToken error
//    //                  that I skip inline whitespace first
//    const result2 = try scan(result1.end, limit);
//    std.debug.assert(@ptrToInt(result2.end) > @ptrToInt(result1.end));
//    // should be impossible to get 2 inline_whitespace tokens in a row
//    if (result2.kind != .inline_whitespace) unreachable;
//    return ScanSkipResult { .start = result1.end, .scan_result = result2 };
//}
//

const BinaryOpKind = enum {
    assign,
    pipe,
    @"or",
    @"and",
    eq,
    gt,
    lt,
};

const binary_builtin_id_map = std.ComptimeStringMap(BinaryOpKind, .{
    .{ "pipe", BinaryOpKind.pipe },
    .{ "or", BinaryOpKind.@"or" },
    .{ "and", BinaryOpKind.@"and" },
    .{ "eq", BinaryOpKind.eq },
    .{ "gt", BinaryOpKind.gt },
    .{ "lt", BinaryOpKind.lt },
});

const NodeKind = enum {
    string,
    binary_op,
    id,
    // inline_cmd,
    // concat, (equivalent to NodeMultiple from python prototype)
};

// TODO: I could probably make this struct much smaller
const Node = struct {
    pos: [*]const u8,
    end: [*]const u8,
    data: union(enum) {
        arg: void,
        assign: void,
        user_id: void,
        builtin_id: void,
        binary_op: BinaryOpKind,
        double_quoted_string: void,
        single_quoted_string: void,
        escape_sequence: void,
    },
    pub fn getKind(self: Node) NodeKind {
        return switch (self.data) {
            .arg, .double_quoted_string, .single_quoted_string, .escape_sequence => .string,
            .assign, .binary_op => .binary_op,
            .user_id, .builtin_id => .id,
        };
    }

    pub fn getIdSlice(self: Node) []const u8 {
        const delim = @as(u8, switch (self.data) {
            .builtin_id, .binary_op => '@',
            .user_id => '$',
            else => unreachable,
        });
        const id_limit = self.end - @as(u1, if ((self.end-1)[0] == delim) 1 else 0);
        return self.pos[1 .. @ptrToInt(id_limit) - @ptrToInt(self.pos)];
    }

    pub fn getStringData(self: Node) []const u8 {
        switch (self.data) {
            .double_quoted_string => {
                return self.pos[1 .. @ptrToInt(self.end) - @ptrToInt(self.pos) - 1];
            },
            else => unreachable,
        }
    }
};


fn parseOneNode(src: [*]const u8, token: LexResult, allstringliterals: bool) !Node {
    _ = allstringliterals;
    switch (token.kind) {
        .inline_whitespace, .comment, .newline, .close_paren => unreachable,
        .builtin_id => {
            var node = Node { .pos = src, .end = token.end, .data = .builtin_id };
            const id = node.getIdSlice();
            if (binary_builtin_id_map.get(id)) |binary_op_kind| {
                node.data = .{ .binary_op = binary_op_kind };
            }
            return node;
        },
        .user_id => return Node { .pos = src, .end = token.end, .data = .user_id },
        .arg => return Node { .pos = src, .end = token.end, .data = .arg },
        .assign_op => return Node { .pos = src, .end = token.end, .data = .assign },
        .double_quoted_string => return Node { .pos = src, .end = token.end, .data = .double_quoted_string },
        .open_paren => @panic("parseOneNode open_paren not impl"),
        .single_quoted_string => @panic("parseOneNode single_quoted_string not impl"),
        .escape_sequence => return Node { .pos = src, .end = token.end, .data = .escape_sequence },
    }
}

test "parseOneNode" {
    {
        const src: []const u8 = "@a";
        const result = try parseOneNode(src.ptr, .{.kind = .builtin_id, .end = src.ptr + src.len}, true);
        try testing.expectEqual(src[1..], result.getIdSlice());
        try testing.expectEqual(NodeKind.id, result.getKind());
    }
    {
        const src: []const u8 = "@a@";
        const result = try parseOneNode(src.ptr, .{.kind = .builtin_id, .end = src.ptr + src.len}, true);
        try testing.expectEqual(@as([]const u8, src[1..2]), result.getIdSlice());
    }
    {
        const src: []const u8 = "@pipe";
        const result = try parseOneNode(src.ptr, .{.kind = .builtin_id, .end = src.ptr + src.len}, true);
        try testing.expectEqual(BinaryOpKind.pipe, result.data.binary_op);
    }
    {
        const src: []const u8 = "$a";
        const result = try parseOneNode(src.ptr, .{.kind = .user_id, .end = src.ptr + src.len}, true);
        try testing.expectEqual(src[1..], result.getIdSlice());
    }
    {
        const src: []const u8 = "$a$";
        const result = try parseOneNode(src.ptr, .{.kind = .user_id, .end = src.ptr + src.len}, true);
        try testing.expectEqual(@as([]const u8, src[1..2]), result.getIdSlice());
    }
    {
        const src: []const u8 = "a";
        _ = try parseOneNode(src.ptr, .{.kind = .arg, .end = src.ptr + src.len}, true);
    }
    {
        const src: []const u8 = "=";
        _ = try parseOneNode(src.ptr, .{.kind = .assign_op, .end = src.ptr + src.len}, true);
    }
    {
        const src: []const u8 = "\"a\"";
        const result = try parseOneNode(src.ptr, .{.kind = .double_quoted_string, .end = src.ptr + src.len}, true);
        try testing.expect(std.mem.eql(u8, "a", result.getStringData()));
    }
    {
        const src: []const u8 = "@@";
        _ = try parseOneNode(src.ptr, .{.kind = .escape_sequence, .end = src.ptr + src.len}, true);
    }
}

const NodeList = std.ArrayList(Node);

const ParseNodeResult = struct {
    node: Node,
    token: ?LexResult,
};

fn parseNode(src: [*]const u8, limit: [*]const u8, token: LexResult, allstringliterals: bool) !ParseNodeResult {
    std.debug.assert(@ptrToInt(token.end) > @ptrToInt(src));
    switch (token.kind) {
        .inline_whitespace, .comment, .newline, .close_paren => unreachable,
        else => {},
    }
    var next = src;
    var next_token = token;

    var opt_node: ?Node = null;

    while (true) {
        {
            //std.debug.print("calling parseOneNode with '{s}'...\n", .{
            //    next[0 .. @ptrToInt(next_token.end) - @ptrToInt(next)]});
            const next_node = try parseOneNode(next, next_token, allstringliterals);
            //std.debug.print("b: {}\n", .{next_node});
            std.debug.assert(@ptrToInt(next_node.end) > @ptrToInt(next_node.pos));
            if (opt_node) |_| {
                @panic("not impl");
            } else {
                opt_node = next_node;
            }
            std.debug.assert(opt_node.?.end == next_node.end);
        }

        if (opt_node.?.end == limit)
            return ParseNodeResult { .node = opt_node.?, .token = null };

        const scan_result = try scan(opt_node.?.end, limit);
        if (scan_result.end == opt_node.?.end) {
            // TODO: then we reached an unrecognized token!!
            @panic("here");
        }
//        std.debug.print("got {}-char token: '{s}'\n", .{@ptrToInt(scan_result.end) - @ptrToInt(next), next[0..@ptrToInt(scan_result.end) - @ptrToInt(next)]});
        if (assert_one_match)
            lex.assertNoMatchAfter(opt_node.?.end, limit, scan_result.kind);
        switch (scan_result.kind) {
            .inline_whitespace, .comment, .newline, .close_paren =>
                return ParseNodeResult { .node = opt_node.?, .token = scan_result },
            else => {},
        }
        next_token = scan_result;
        next = scan_result.end;
    }
}

test "parseNode" {
    {
        const src: []const u8 = "a";
        _ = try parseNode(src.ptr, src.ptr + src.len, .{ .kind = .arg, .end = src.ptr + src.len}, true);
    }
}

// need a NodeBuilder
pub fn parseCommand(nodes: *NodeList, src: [*]const u8, limit: [*]const u8, allstringliterals: bool) ![*]const u8 {
    _ = nodes;
    _ = allstringliterals;

    var next = lex.lexInlineWhitespace(src, limit);
    const token = try scan(next, limit);
    if (token.end == next)
        return src;
    if (assert_one_match)
        lex.assertNoMatchAfter(src, limit, token.kind);

    while (true) {
        if (token.kind == .comment or token.kind == .newline)
            return token.end;
        if (token.kind == .close_paren)
            return next; // token.pos
        //const result = try parseNode(next, limit, allstringliterals);
        //_ = result;
        @panic("not impl");
    }
}

fn testParseCommand(src: []const u8) !void {
    var nodes = NodeList.init(std.testing.allocator);
    defer nodes.deinit();
    _ = try parseCommand(&nodes, src.ptr, src.ptr + src.len, true);
}

test "parseCommand" {
    //try testParseCommand("a");
}
