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
    var kind = lex.token0;
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
    //assign,
    pipe,
    @"or",
    @"and",
    eq,
    gt,
    lt,
};

const binary_builtin_id_map = std.ComptimeStringMap(BinaryOpKind, .{
    .{ "pipe", .pipe },
    .{ "or", .@"or" },
    .{ "and", .@"and" },
    .{ "eq", .eq },
    .{ "gt", .gt },
    .{ "lt", .lt },
});

const NodeKind = enum {
    //arg_sep,
    string,
    binary_op,
    id,
    inline_cmd_start,
    inline_cmd_end,
    // concat, (equivalent to NodeMultiple from python prototype)
};

// TODO: I could probably make this struct much smaller
pub const Node = struct {
    pos: [*]const u8,
    //end: [*]const u8,
    join_prev: bool, // if true, join this node with the previous node
    data: union(enum) {
        //arg_sep: void,
        arg: [*]const u8,
        assign: void,
        user_id: void,
        builtin_id: [*]const u8,
        binary_op: BinaryOpKind,
        double_quoted_string: [*]const u8,
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
        const data: struct {delim:u8, end: [*]const u8} = switch (self.data) {
            .builtin_id => |d| .{ .delim = '@', .end = d },
            .binary_op => |_| .{ .delim = '@', .end = @panic("not impl") },
            .user_id => |_| .{ .delim = '$', .end = @panic("not impl") },
            else => unreachable,
        };
        const id_limit = data.end - @as(u1, if ((data.end-1)[0] == data.delim) 1 else 0);
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

// Command Memory Layout
//
// every node has a Xbit (16?) offset into the command string that points
// to the START of the first token of the node.  This is important because
// each node will have access to both it's start and the start of the next token.
// NOTE: this could be usize to support any size of platform?
//
// string (tokens: arg, double_quoted_string, single_quoted_string, escape_sequence)
// id (tokens: builtin_id, user_id)
// binary_op (tokens: assign, builtin_id)
// inline_cmd_start (token: open_paren)
// inline_cmd_end (token: close_paren)
// arg_separator (token: inline_whitespace)
//
//
//const NodeKind = enum {
//    string, id, binary_op, inline_cmd_start, inline_cmd_end, arg_separator,
//};
//const CmdOffset = u16;
//const Node = struct {
//    //token_start: CmdOffset,
//    pos: [*]const u8,
//    kind: NodeKind,

//    // TODO: remove this method
//    pub fn getKind(self: Node) NodeKind { return self.kind; }
//};


pub const NodeBuilder = @import("block_list.zig").BlockList(Node, .{});

fn parseOneNode(src: [*]const u8, token: LexResult, allstringliterals: bool) Node {
    _ = allstringliterals;
    switch (token.kind) {
        .inline_whitespace, .comment, .newline, .close_paren => unreachable,
        .builtin_id => {
            var node = Node { .pos = src, //.end = token.end,
                .data = .{ .builtin_id = token.end } };
            const id = node.getIdSlice();
            if (binary_builtin_id_map.get(id)) |binary_op_kind| {
                node.data = .{ .binary_op = binary_op_kind };
            }
            return node;
        },
        .user_id => return Node { .pos = src, //.end = token.end,
            .data = .user_id },
        .arg => return Node { .pos = src, //.end = token.end,
            .data = .{ .arg = token.end } },
        .assign_op => return Node { .pos = src, //.end = token.end,
            .data = .assign },
        .double_quoted_string => return Node { .pos = src, //.end = token.end,
            .data = .{ .double_quoted_string = token.end } },
        .open_paren => @panic("parseOneNode open_paren not impl"),
        .single_quoted_string => @panic("parseOneNode single_quoted_string not impl"),
        .escape_sequence => return Node { .pos = src, //.end = token.end,
            .data = .escape_sequence },
    }
}

test "parseOneNode" {
//    {
//        const src: []const u8 = "@a";
//        const result = parseOneNode(src.ptr, .{.kind = .builtin_id, .end = src.ptr + src.len}, true);
//        //try testing.expectEqual(src[1..], result.getIdSlice());
//        try testing.expectEqual(NodeKind.id, result.getKind());
//    }
//    {
//        const src: []const u8 = "@a@";
//        const result = try parseOneNode(src.ptr, .{.kind = .builtin_id, .end = src.ptr + src.len}, true);
//        try testing.expectEqual(@as([]const u8, src[1..2]), result.getIdSlice());
//    }
//    {
//        const src: []const u8 = "@pipe";
//        const result = try parseOneNode(src.ptr, .{.kind = .builtin_id, .end = src.ptr + src.len}, true);
//        try testing.expectEqual(BinaryOpKind.pipe, result.data.binary_op);
//    }
//    {
//        const src: []const u8 = "$a";
//        const result = try parseOneNode(src.ptr, .{.kind = .user_id, .end = src.ptr + src.len}, true);
//        try testing.expectEqual(src[1..], result.getIdSlice());
//    }
//    {
//        const src: []const u8 = "$a$";
//        const result = try parseOneNode(src.ptr, .{.kind = .user_id, .end = src.ptr + src.len}, true);
//        try testing.expectEqual(@as([]const u8, src[1..2]), result.getIdSlice());
//    }
//    {
//        const src: []const u8 = "a";
//        _ = try parseOneNode(src.ptr, .{.kind = .arg, .end = src.ptr + src.len}, true);
//    }
//    {
//        const src: []const u8 = "=";
//        _ = try parseOneNode(src.ptr, .{.kind = .assign_op, .end = src.ptr + src.len}, true);
//    }
//    {
//        const src: []const u8 = "\"a\"";
//        const result = try parseOneNode(src.ptr, .{.kind = .double_quoted_string, .end = src.ptr + src.len}, true);
//        try testing.expect(std.mem.eql(u8, "a", result.getStringData()));
//    }
//    {
//        const src: []const u8 = "@@";
//        _ = try parseOneNode(src.ptr, .{.kind = .escape_sequence, .end = src.ptr + src.len}, true);
//    }
}

const ParseNodeResult = struct {
    node: Node,
    token: ?LexResult,
};

fn parseNode(
    node_builder: *NodeBuilder,
    src: [*]const u8,
    limit: [*]const u8,
    token: LexResult,
    allstringliterals: bool,
) !ParseNodeResult {

    std.debug.assert(@ptrToInt(token.end) > @ptrToInt(src));
    switch (token.kind) {
        .inline_whitespace, .comment, .newline, .close_paren => unreachable,
        else => {},
    }
    var next = src;
    var next_token = token;

    while (true) {
        _ = node_builder; // TODO: remove this!!!
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
    var node_builder = NodeBuilder { .allocator = std.testing.allocator };
    node_builder.deinit();
    {
//        const src: []const u8 = "a";
//        _ = try parseNode(&node_builder, src.ptr, src.ptr + src.len, .{ .kind = .arg, .end = src.ptr + src.len}, true);
    }
}


const ParseCommandResult = union(enum) {
    success: [*]const u8,
    unknown_token: [*]const u8,
    err: error {OutOfMemory},
};
pub fn parseCommand(node_builder: *NodeBuilder, src: [*]const u8, limit: [*]const u8, allstringliterals: bool) ParseCommandResult {
    _ = allstringliterals;

    std.debug.assert(@ptrToInt(src) <= @ptrToInt(limit));

    var next_node_is_joined = false;
    var next = src;

    while (true) : (next_node_is_joined = true) {
        if (next != limit) {
            const after_whitespace = lex.lexInlineWhitespace(next, limit);
            if (after_whitespace != next) {
                next_node_is_joined = false;
                next = after_whitespace;
            }
        }
        if (next == limit)
            return .{ .success = limit };
        const token = scan(next, limit) catch |e| switch (e) {
            error.UnknownToken => return .{ .unknown_token = next },
        };
        if (@ptrToInt(token.end) <= @ptrToInt(next)) unreachable;
        if (assert_one_match)
            lex.assertNoMatchAfter(next, limit, token.kind);
        const token_start = next;
        next = token.end;

        switch (token.kind) {
            .inline_whitespace => unreachable, // should be handled by lexInlineWhitespace above
            .comment, .newline => return .{ .success = token.end },
            .close_paren => return .{ .success = token_start },
            .assign_op => {
                node_builder.append(.{ .pos = token_start, .join_prev = next_node_is_joined,
                    .data = .assign, }) catch |e| switch (e) {
                    error.OutOfMemory => return .{ .err = e },
                };
            },
            .builtin_id => {
                node_builder.append(.{ .pos = token_start, .join_prev = next_node_is_joined,
                    .data = .{ .builtin_id = token.end }, }) catch |e| switch (e) {
                    error.OutOfMemory => return .{ .err = e },
                };
            },
            .user_id => @panic("here"),
            .arg => {
                node_builder.append(.{ .pos = token_start, .join_prev = next_node_is_joined,
                    .data = .{ .arg = token.end }, }) catch |e| switch (e) {
                    error.OutOfMemory => return .{ .err = e },
                };
            },
            .double_quoted_string => {
                node_builder.append(.{ .pos = token_start, .join_prev = next_node_is_joined,
                    .data = .{ .double_quoted_string = token.end }, }) catch |e| switch (e) {
                    error.OutOfMemory => return .{ .err = e },
                };
            },
            .single_quoted_string => @panic("here"),
            .escape_sequence => {
                node_builder.append(.{ .pos = token_start, .join_prev = next_node_is_joined,
                    .data = .escape_sequence, }) catch |e| switch (e) {
                    error.OutOfMemory => return .{ .err = e },
                };
            },
            .open_paren => {
                @panic("not impl");
            },
        }
    }
}

fn testParseCommand(src: []const u8) !void {
    var node_builder = NodeBuilder { .allocator = std.testing.allocator };
    node_builder.deinit();
    switch(parseCommand(&node_builder, src.ptr, src.ptr + src.len, true)) {
        .success => {
            // TODO: verify entire command was parsed
        },
        .unknown_token => return error.UnknownToken,
        .err => |e| return e,
    }
}

test "parseCommand" {
    try testParseCommand(" ");
    try testParseCommand("\t  \t");
    //try testParseCommand("a");
}
