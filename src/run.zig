const std = @import("std");
const testing = std.testing;
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;

const lex = @import("lex.zig");

const assert_one_match = true;

const OutKind = enum { out, err };

const RunHooks = struct {
    allocator: *std.mem.Allocator,
    onError: fn(base: *RunHooks, pos: [*]const u8, msg: []const u8) void,
    onWrite: fn(base: *RunHooks, kind: OutKind, bytes: []const u8) WriteError!usize,

    const WriteError = error {OutOfMemory};
    const WriterContext = struct {
        hooks: *RunHooks,
        kind: OutKind,
        fn write(self: WriterContext, bytes: []const u8) WriteError!usize {
            return self.hooks.onWrite(self.hooks, self.kind, bytes);
        }
    };
    pub const Writer = std.io.Writer(WriterContext, WriteError, WriterContext.write);
    fn print(self: *RunHooks, out_kind: OutKind, comptime fmt: []const u8, args: anytype) !void {
        return std.fmt.format(Writer {
            .context = .{
                .hooks = self,
                .kind = out_kind,
            },
        }, fmt, args);
    }

    fn reportError(self: *RunHooks, pos: [*]const u8, comptime fmt: []const u8, args: anytype) void {
        var msg_buf: [200]u8 = undefined;
        var fbs = std.io.fixedBufferStream(&msg_buf);
        std.fmt.format(fbs.writer(), fmt, args) catch |e| switch (e) {
            error.NoSpaceLeft => {
                //std.debug.panic("error message exceeded {} bytes", .{msg_buf.len});
                self.onError(self, pos, "error message too long");
                return;
            },
        };
        self.onError(self, pos, fbs.getWritten());
    }
};

const LexResult = struct {
    kind: lex.TokenKind,
    end: [*]const u8,
};
fn scan(hooks: *RunHooks, text: [*]const u8, limit: [*]const u8) RunFailedError!LexResult {
    std.debug.assert(@ptrToInt(text) < @ptrToInt(limit));
    var kind = lex.token0;
    const end = lex.lex(text, limit, &kind);
    if (end != text) {
        if (assert_one_match)
            lex.assertNoMatchAfter(text, limit, kind);
        return LexResult { .kind = kind, .end = end };
    }

    // figure out what went wrong
//    # time to try to figure out what went wrong
//    c = next.charAt(0)
//    if c == ord('('):
//        raise SyntaxError(pos, "missing close paren for: {}".format(previewStringPtr(next, limit, 30)))
//    if c == ord('"'):
//        raise SyntaxError(pos, "missing double-quote to close: {}".format(previewStringPtr(next, limit, 30)))
//    next_str = next.toStringWithLimit(limit)
//    for seq in (b"''''''", b"'''''", b"''''", b"'''", b"''", b"'"):
//        if next_str.startswith(seq):
//            phrase = "single-quote" if (len(seq) == 1) else "{} single-quote sequence".format(len(seq))
//            raise SyntaxError(pos, "missing {} to close: {}".format(phrase, previewStringPtr(next, limit, 30)))
//
//    # I think we need at most 2 characters to see what went wrong
//    bad_str = next_str[:min(limit.subtract(next), 2)]
//    raise SyntaxError(pos, "unrecognized character sequence '{}'".format(bad_str.decode('ascii')))
//
    if (text[0] == '(') {
//        //hooks.onSyntaxError("missing close paren for: {}", .{previewStringPtr(text, limit, 30)});
//        hooks.onSyntaxError(SyntaxError.MissingCloseParen
        @panic("here");
    }
    if (text[0] == '"') {
        @panic("here");
    }
    if (text[0] == '\'') {
        @panic("here");
    }

    const bad_str = text[0..std.math.min(2, @ptrToInt(limit)-@ptrToInt(text))];
    hooks.reportError(text, "unrecognized character sequence '{s}'", .{bad_str});
    return error.RunFailed;
}

const RunFailedError = error { RunFailed } || std.mem.Allocator.Error;
pub fn runSlice(hooks: *RunHooks, s: []const u8) RunFailedError![*]const u8 {
    return run(hooks, s.ptr, s.ptr + s.len);
}
pub fn run(hooks: *RunHooks, start: [*]const u8, limit: [*]const u8) RunFailedError![*]const u8 {
    var next = lex.lexInlineWhitespace(start, limit);
    if (next == limit)
        return limit;

    const first_token = try scan(hooks, next, limit);

    if (first_token.kind == .builtin_id) {
        return runBuiltin(hooks, next, first_token.end, limit);
    }
    _ = first_token;

    //@panic("here");
    return next;
    //while (true) {
    //}
}


//const StitchObj = union(enum) {
//    bool: bool,
//};
//const RunBuiltinResult = struct {
//    end: [*]const u8,
//    return_obj: StitchObj,
//};
fn runBuiltin(
    hooks: *RunHooks,
    builtin_start: [*]const u8,
    builtin_end: [*]const u8,
    limit: [*]const u8
) RunFailedError![*]const u8 {
    std.debug.assert(builtin_start[0] == '@');
    var builtin_id_len = @ptrToInt(builtin_end) - @ptrToInt(builtin_start);
    if (@intToPtr([*]const u8, @ptrToInt(builtin_end) - 1)[0] == '@') {
        builtin_id_len -= 1;
    }
    
    const builtin_id = builtin_start[1.. builtin_id_len];

    //if (std.mem.eql(u8, builtin_id, "false")) {
    //    !
    //}


    // for now we'll assume all builtins just take string arguments
    var args = ArrayList([]const u8).init(hooks.allocator);
    defer {
        for (args.items) |arg| {
            if (!ptrIntersects(arg.ptr, builtin_end, limit)) {
                hooks.allocator.free(arg);
            }
        }
        args.deinit();
    }

    const end = try allocArgStrings(hooks, &args, builtin_end, limit);
    
    if (std.mem.eql(u8, builtin_id, "echo")) {
        if (args.items.len == 0) {
            try hooks.print(.out, "\n", .{});
        } else {
            for (args.items[0 .. args.items.len - 1]) |arg| {
                try hooks.print(.out, "{s}", .{arg});
            }
            try hooks.print(.out, "{s}\n", .{args.items[args.items.len-1]});
        }
        return end;
    } else {
        std.debug.panic("builtin '@{s}' not impl", .{builtin_id});
    }
}

fn ptrIntersects(s: [*]const u8, start: [*]const u8, limit: [*]const u8) bool {
    return @ptrToInt(s) < @ptrToInt(limit) and @ptrToInt(s) >= @ptrToInt(start);
}


const ArgBuilder = struct {
    state: union(enum) {
        empty: void,
        source: []const u8,
        building: ArrayListUnmanaged(u8),
    } = .empty,

    pub fn errDeinit(self: *ArgBuilder, allocator: *std.mem.Allocator) void {
        switch (self.state) {
            .empty, .source => {},
            .building => |*b| b.deinit(allocator),
        }
    }
    pub fn flush(self: *ArgBuilder, allocator: *std.mem.Allocator, args: *ArrayList([]const u8)) !void {
        switch (self.state) {
            .empty => {},
            .source => |s| {
                try args.append(s);
                self.state = .empty;
            },
            .building => |al| {
                const result = allocator.shrink(al.items.ptr[0..al.capacity], al.items.len);
                std.debug.assert(result.len == al.items.len);
                try args.append(result);
                self.state = .empty;
            },
        }
    }

    pub fn add(self: *ArgBuilder, allocator: *std.mem.Allocator, arg: []const u8) !void {
        switch (self.state) {
            .empty => {
                self.state = .{ .source = arg };
            },
            .source => |s| {
                const min_size = s.len + arg.len;
                var mem = try allocator.allocAdvanced(u8, null, min_size, .at_least);
                @memcpy(mem.ptr, s.ptr, s.len);
                @memcpy(mem.ptr + s.len, arg.ptr, arg.len);
                self.state = .{ .building = ArrayListUnmanaged(u8) {
                    .items = mem[0..min_size],
                    .capacity = mem.len,
                }};
            },
            .building => |*al| {
                try al.appendSlice(allocator, arg);
            },
        }
    }
};

fn allocArgStrings(hooks: *RunHooks, args: *ArrayList([]const u8), start: [*]const u8, limit: [*]const u8) RunFailedError![*]const u8 {
    if (start == limit) return limit;
    var next = lex.lexInlineWhitespace(start, limit);

    var arg_builder = ArgBuilder { };
    errdefer arg_builder.errDeinit(hooks.allocator);
    
    while (true) {
        if (next == limit) {
            try arg_builder.flush(hooks.allocator, args);
            return limit;
        }
        const token = try scan(hooks, next, limit);
        switch (token.kind) {
            .inline_whitespace => {
                try arg_builder.flush(hooks.allocator, args);
            },
            .comment, .newline => {
                try arg_builder.flush(hooks.allocator, args);
                return token.end;
            },
            .close_paren => {
                try arg_builder.flush(hooks.allocator, args);
                return next;
             },
            .assign_op => {
                @panic("not impl");
            },
            .arg => {
                try arg_builder.add(hooks.allocator, next[0 .. @ptrToInt(token.end) - @ptrToInt(next)]);
            },
            .builtin_id, .user_id => {
                @panic("not impl");
            },
            .double_quoted_string => {
                try arg_builder.add(hooks.allocator, next[1 .. @ptrToInt(token.end) - @ptrToInt(next) - 1]);
            },
            .single_quoted_string,
            .escape_sequence => {
               try arg_builder.add(hooks.allocator, next[1..2]);
            },
            .open_paren => {
                @panic("not impl");
            },
        }
        next = token.end;
    }
}

const TestHooks = struct {
    hooks: RunHooks,
    stdout: ArrayList(u8),
    stderr: ArrayList(u8),

    result: union(enum) {
        none: void,
          err: struct {
              pos: [*]const u8,
              msg: []const u8,
          },
    },

    pub fn init() TestHooks {
        return .{
            .hooks = .{
                .allocator = testing.allocator,
                .onError = onError,
                .onWrite = onWrite,
            },
            .stdout = ArrayList(u8).init(testing.allocator),
            .stderr = ArrayList(u8).init(testing.allocator),
            .result = .none,
        };
    }

    pub fn deinit(self: *TestHooks) void {
        self.stdout.deinit();
        self.stderr.deinit();
        switch (self.result) {
            .none => {},
            .err => |err| {
                self.hooks.allocator.free(err.msg);
            },
        }
    }

    fn onError(base: *RunHooks, pos: [*]const u8, msg: []const u8) void {
        const self = @fieldParentPtr(TestHooks, "hooks", base);
        std.debug.assert(self.result == .none);
        self.result = .{ .err = .{
            .pos = pos,
            .msg = self.hooks.allocator.dupe(u8, msg) catch |e| switch (e) {
                error.OutOfMemory => {
                    std.debug.panic("failed to allocate memory for the error message '{s}'", .{msg});
                },
            },
        }};
    }

    fn onWrite(base: *RunHooks, out_kind: OutKind, bytes: []const u8) RunHooks.WriteError!usize {
        const self = @fieldParentPtr(TestHooks, "hooks", base);
        const out = switch (out_kind) { .out => &self.stdout, .err => &self.stderr };
        try out.appendSlice(bytes);
        return bytes.len;
    }
};

const TestConfig = struct {
    err: ?[]const u8 = null,
    stdout: ?[]const u8 = null,
    stderr: ?[]const u8 = null,
    stdin: ?[]const u8 = null,
};

fn runTest(src: []const u8, config: TestConfig) !void {
    var hooks = TestHooks.init();
    defer hooks.deinit();

    if (config.stdin) |stdin| {
        _ = stdin;
        @panic("not impl");
    }
    const end = runSlice(&hooks.hooks, src) catch |e| switch (e) {
        error.OutOfMemory => return e,
        error.RunFailed => {
            const err = switch (hooks.result) {
                .none => @panic("RunFailed but no error was reported, this should be impossible"),
                .err => |err| err,
            };
            if (config.err) |expected_error| {
                if (!std.mem.eql(u8, expected_error, err.msg)) {
                    std.debug.print("\nerror: error message mismatch\nexpected: '{s}'\nactual  : '{s}'\n", .{expected_error, err.msg});
                    return error.TestUnexpectedResult;
                }
            } else {
                std.debug.print("error: unexpected failure: {s}\n", .{err.msg});
                return error.TestUnexpectedResult;
            }

            if (config.stdout) |stdout| {
                _ = stdout;
                @panic("not impl");
            }
            if (config.stderr) |stderr| {
                if (!std.mem.eql(u8, stderr, hooks.stderr.items)) {
                    std.debug.print("\nerror: stderr mismatch\nexpected: '{s}'\nactual  : '{s}'\n", .{stderr, hooks.stderr.items});
                    return error.TestUnexpectedResult;
                }
            }
            //@panic("not impl");
            return;
        },
    };
    _ = end;

    if (config.err) |err| {
        std.debug.print("\ndid not get expected error '{s}'\n", .{err});
        return error.TestUnexpectedResult;
    }

    if (config.stdout) |stdout| {
        if (!std.mem.eql(u8, stdout, hooks.stdout.items)) {
            std.debug.print("\nerror: stdout mismatch\nexpected: '{s}'\nactual  : '{s}'\n", .{stdout, hooks.stdout.items});
            return error.TestUnexpectedResult;
        }
    } else {
        try testing.expect(hooks.stdout.items.len == 0);
    }

    if (config.stderr) |stderr| {
        _ = stderr;
        @panic("todo");
    } else if (hooks.stderr.items.len > 0) {
        @panic("todo");
    }
}


test {
    try runTest("@echo", .{.stdout = "\n"});
    try runTest("@echo@", .{.stdout = "\n"});
    try runTest("@echo hello", .{.stdout = "hello\n"});
    try runTest("@echo@ hello", .{.stdout = "hello\n"});
    try runTest("@echo \"a string!\"", .{.stdout = "a string!\n"});
    try runTest("@echo \"string and \"arg\" put together\"", .{.stdout = "string and arg put together\n"});
}

test "ported from python test" {
//    testSyntaxError(b"(", "missing close paren for: (")
//    testSyntaxError(b"(foo", "missing close paren for: (foo")
//    testSyntaxError(b"(a long command that demonstrates we shouldnt print this whole thing when we show an error",
//                    "missing close paren for: (a long command that demonstra[..snip..]")
    try runTest("$$", .{.err = "unrecognized character sequence '$$'"});
    try runTest("@`", .{.err = "unrecognized character sequence '@`'"});
    try runTest("@echo @@", .{.stdout = "@\n"});
    try runTest("@echo @#", .{.stdout = "#\n"});
    try runTest("@echo @$", .{.stdout = "$\n"});
    try runTest("@echo @)", .{.stdout = ")\n"});
    try runTest("@echo @(", .{.stdout = "(\n"});
    try runTest("@echo @=", .{.stdout = "=\n"});
    try runTest("@echo @\"", .{.stdout = "\"\n"});
    try runTest("@echo @'", .{.stdout = "'\n"});
//
//    testSemanticError(b"@multiline", "@multiline requires at least 1 argument")
//    testSemanticError(b"@multiline @true", "the @multiline builtin is only supported within an (..inline command..)")
//    testSemanticError(b"(@multiline @true @and @true)", "@multiline does not accept Bool")
//
//    testSemanticError(b"@false", "unhandled Bool")
//    testSemanticError(b"@true", "unhandled Bool")
//
//    testSemanticError(b"@true dat", "unexpected Bool at the start of a command")
//
    try runTest("@echo", .{.stdout = "\n"});
    try runTest("@echo foo", .{.stdout = "foo\n"});
    try runTest("@echo \"foo\"", .{.stdout = "foo\n"});
//
//    #
//    # double-quoted string literals
//    #
//    testSyntaxError(b'"', 'missing double-quote to close: "')
//    testSyntaxError(b'@echo "foo', 'missing double-quote to close: "foo')
//    testSyntaxError(b'@echo "01234567890123456789012345678',
//                    'missing double-quote to close: "01234567890123456789012345678')
//    testSyntaxError(b'@echo "012345678901234567890123456789',
//                    'missing double-quote to close: "01234567890123456789012345678[..snip..]')
//    testCommand(b'@echo @"', 0, b'"\n')
//    testCommand(b'@echo "#@$()"', 0, b'#@$()\n')
//
//    #
//    # single-quoted string literals
//    #
//    testSyntaxError(b"'", "missing single-quote to close: '")
//    testSyntaxError(b"''", "missing 2 single-quote sequence to close: ''")
//    testSyntaxError(b"'''", "missing 3 single-quote sequence to close: '''")
//    testSyntaxError(b"''''", "missing 4 single-quote sequence to close: ''''")
//    testSyntaxError(b"'''''", "missing 5 single-quote sequence to close: '''''")
//    testSyntaxError(b"''''''", "missing 6 single-quote sequence to close: ''''''")
//    testSyntaxError(b"'''''''", "missing 6 single-quote sequence to close: '''''''")
//    testSyntaxError(b"'a", "missing single-quote to close: 'a")
//    testSyntaxError(b"''a", "missing 2 single-quote sequence to close: ''a")
//    testSyntaxError(b"'''a", "missing 3 single-quote sequence to close: '''a")
//    testSyntaxError(b"''''a", "missing 4 single-quote sequence to close: ''''a")
//    testSyntaxError(b"@echo 'foo", "missing single-quote to close: 'foo")
//    testSyntaxError(b"@echo '01234567890123456789012345678",
//                    "missing single-quote to close: '01234567890123456789012345678")
//    testSyntaxError(b"@echo '012345678901234567890123456789",
//                    "missing single-quote to close: '01234567890123456789012345678[..snip..]")
//    testCommand(b'@echo "#@$()"', 0, b"#@$()\n")
//
//    testSyntaxError(b"@echo '\n", "missing single-quote to close: '")
//    testSyntaxError(b"@echo ''\n''", "missing 2 single-quote sequence to close: ''")
//    testCommand(b"@echo '''\n'''", 0, b"")
//    testCommand(b"@echo '''\nhello'''", 0, b"hello\n")
//    testCommand(b"@echo '''\nhello\nworld'''", 0, b"hello\nworld\n")
//    testCommand(b"@echo '''\n\"hello\"\n'world''''", 0, b"\"hello\"\n'world'\n")
//
//    testCommand(b"@echo 'hello\"'", 0, b"hello\"\n")
//    testCommand(b"@echo ''hello\"'''", 0, b"hello\"'\n")
//    testCommand(b"@echo '''hello\"''''", 0, b"hello\"'\n")
//    testCommand(b"@echo ''''hello\"'''''", 0, b"hello\"'\n")
//    testCommand(b"@echo '''''hello\"''''''", 0, b"hello\"'\n")
//    testCommand(b"@echo ''''''hello\"'''''''", 0, b"hello\"'\n")
//
//    # should be a syntax error because there are no quotes!
//    testSyntaxError(b"@echo 'foo'", "got a single-quote string literal without double-quotes nor newlines, use double quotes instead or invoke @allstringliterals")
//    testCommand(b"@allstringliterals\n@echo 'foo'", 0, b"foo\n")
//    testCommand(b"\n".join([
//        b"name = joe",
//        b"age = 64",
//        b"@echo '''\nhello '''$name''', you're\n64!!!'''"]
//    ), 0, b"hello joe, you're\n64!!!\n")
//
//    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//    # TODO: test all the inline command prefixes
//    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//    testSemanticError(b"@exitcode true", "the @exitcode builtin is only supported within an (..inline command..)")
//    testSemanticError(b"@assert (@exitcode)", "@exitcode requires at least 1 argument")
//    testSemanticError(b"@assert (@exitcode @exitcode true)", "@exitcode is not compatible with @exitcode")
//    testSemanticError(b"@assert (@exitcode true @exitcode)", "cannot coerce Builtin '@exitcode' to String")
//    testSemanticError(b"@assert (@exitcode @true)", "unhandled Bool")
//    testSemanticError(b"@assert (@exitcode @multiline true)", "got @multiline but stdout is not being captured?  what's going on?")
//    testCommand(b"@assert (@exitcode true)", 0, b"")
//    testCommand(b"@assert @not (@exitcode false)", 0, b"")
//
//    testSyntaxError(b"1@and", "'@and' requires space separation")
//    testSyntaxError(b"@and=", "'@and' requires space separation")
//    testSyntaxError(b"=@and", "'=' requires space separation")
//    testSemanticError(b"@and", "unexpected '@and'")
//    testSemanticError(b"@or", "unexpected '@or'")
//    testSemanticError(b"true @and true", "'@and' does not accept objects of type String")
//    testSemanticError(b"(@array) @and @true", "'@and' does not accept objects of type Array")
//    testSemanticError(b"@true @and (@array)", "'@and' does not accept objects of type Array")
//    testError(b"@false @and",
//              SemanticError, "missing operand after '@and'",
//              SemanticError, "unhandled Bool whose value is False")
//    testSemanticError(b"(true) @and", "'@and' does not accept objects of type String")
//    testError(b"(false) @and",
//              SemanticError, "'@and' does not accept objects of type String",
//              NonZeroExitCodeError, "command failed with exit code 1")
//    testSemanticError(b"false false @or", "unexpected '@or'")
//    testError(b"(false) @or",
//              SemanticError, "'@or' does not accept objects of type String",
//              NonZeroExitCodeError, "command failed with exit code 1")
//    testSemanticError(b"(@multiline true) @or", "'@or' does not accept objects of type String")
//    testSemanticError(b"(@and)", "unexpected '@and'")
//    testSemanticError(b"(@and) @or", "unexpected '@and'")
//    testError(b"@false @and false false",
//              SemanticError, "'@and' does not accept objects of type String",
//              SemanticError, "unhandled Bool whose value is False")
//    testError(b"@false @and @false false",
//              SemanticError, "expected '@and' operator but got token 'false'; commands must be wrapped with (...)",
//              SemanticError, "unhandled Bool whose value is False")
//    testError(b"@false @and @false @or @false",
//              SemanticError, "'@and' and '@or' cannot be chained",
//              SemanticError, "unhandled Bool whose value is False")
//    testSemanticError(b"true @and true", "'@and' does not accept objects of type String")
//    testSemanticError(b"@true @and true", "'@and' does not accept objects of type String")
//    testSemanticError(b"true @and (true)", "'@and' does not accept objects of type String")
//    testSemanticError(b"@false @or @false @and @false", "'@or' and '@and' cannot be chained")
//    testError(b"@true @or @true @and @true",
//              SemanticError, "'@or' and '@and' cannot be chained",
//              SemanticError, "unhandled Bool whose value is True")
//    testSemanticError(b"@true @and @true @or @true", "'@and' and '@or' cannot be chained")
//    testSemanticError(b"@false @or @false @or false", "'@or' does not accept objects of type String")
//
//    # TODO: implement stderr error messages
//    testCommand(b"false", 1, b"")
//    testError(b"@false @or (false)",
//              SemanticError, "'@or' does not accept objects of type String",
//              NonZeroExitCodeError, "command failed with exit code 1")
//    testError(b"(false) @or @false",
//              SemanticError, "'@or' does not accept objects of type String",
//              NonZeroExitCodeError, "command failed with exit code 1")
//    testError(b"(false) @or (false)",
//              SemanticError, "'@or' does not accept objects of type String",
//              NonZeroExitCodeError, "command failed with exit code 1")
//    testSemanticError(b"(true) @or (true)", "'@or' does not accept objects of type String")
//    testError(b"(false) @or (true)",
//              SemanticError, "'@or' does not accept objects of type String",
//              NonZeroExitCodeError, "command failed with exit code 1")
//    testBoolExpression(b"(@exitcode false) @or (@exitcode false)", False)
//    testBoolExpression(b"(@exitcode false) @or (@exitcode true)", True)
//    testBoolExpression(b"@true @or @undefined", True)
//    testBoolExpression(b"@true @or $undefined", True)
//    testBoolExpression(b"(@exitcode true) @or (@exitcode @undefined)", True)
//    testSemanticError(b"@true @and ($missing)", "'$missing' is undefined")
//    testSemanticError(b"(true) @and (false)", "'@and' does not accept objects of type String")
//    testBoolExpression(b"(@exitcode true) @and (@exitcode false)", False)
//    testSemanticError(b"(true) @and (true)", "'@and' does not accept objects of type String")
//    testBoolExpression(b"(@exitcode true) @and (@exitcode true)", True)
//    testBoolExpression(b"(@exitcode false) @or (@exitcode false) @or (@exitcode false)", False)
//    testBoolExpression(b"(@exitcode false) @or (@exitcode false) @or (@exitcode true)", True)
//    testBoolExpression(b"(@exitcode true) @and (@exitcode true) @and (@exitcode false)", False)
//    testBoolExpression(b"(@exitcode true) @and (@exitcode true) @and (@exitcode true)", True)
//
//    testBoolExpression(b"@false @and @false", False)
//    testBoolExpression(b"@false @and @true", False)
//    testBoolExpression(b"@true @and @false", False)
//    testBoolExpression(b"@true @and @true", True)
//
//    testBoolExpression(b"@false @or @false", False)
//    testBoolExpression(b"@false @or @true", True)
//    testBoolExpression(b"@true @or @false", True)
//    testBoolExpression(b"@true @or @true", True)
//
//    testCommand(b"(@echo true)", 0, b"")
//
//    testSemanticError(b"$missing @and @true", "'$missing' is undefined")
//    testSemanticError(b"(@echo hello) @and @true", "'@and' does not accept objects of type String")
//    testCommand(b"@assert (@exitcode @echo hello) @and @true", 0, b"hello\n")
//
//    testSemanticError(b"((@echo true)) @and @true", "'@and' does not accept objects of type String")
//    testBoolExpression(b"(@exitcode (@echo true)) @and @true", True)
//
//    testSemanticError(b"abc@false @and @true", "cannot coerce Bool to String")
//    testSemanticError(b"abc@scriptfile @and @true", "'@and' does not accept objects of type String")
//
//    testSemanticError(b"@assert (@exitcode @true @and @true)", "@exitcode is not compatible with binary expressions")
//    # todo: update this test when @stderr is defined
//    testSemanticError(b"@assert (@stderr @true @and @true)", "'@stderr' is undefined")
//
//    testSemanticError(b"@assert (@exitcode @haveprog foo)", "@exitcode is not compatible with @haveprog")
//
//    testBoolExpression(b"((@exitcode true) @and (@exitcode true)) @and (@exitcode true)", True)
//    testBoolExpression(b"((@exitcode true) @and (@exitcode true)) @and ((@exitcode true) @and (@exitcode false))", False)
//
//    testBoolExpression(b"foo @eq foo", True)
//    testBoolExpression(b"foo @eq bar", False)
//    testSemanticError(b"foo @eq @false", "'@eq' does not accept objects of type Bool")
//    testBoolExpression(b"(@echo foo) @eq foo", True)
//    testBoolExpression(b"(@echo foo) @eq bar", False)
//
//    testSemanticError(b"@assert", "@assert requires at least 1 argument")
//    testError(b"@assert (@array)",
//              SemanticError, "@assert expects a Bool but got a CommandResult",
//              CommandWithNoArgumentsError, "got a command with no arguments")
//    testAssertError(b"@assert @false")
//    testCommand(b"@assert @true", 0, b"")
//    testSemanticError(b"@assert true", "@assert expects a Bool but got a CommandResult")
//
//    testSemanticError(b"@not", "@not requires at least 1 argument")
//    testSemanticError(b"@not @true @and @false", "got binary expression inside ambiguous operator '@not', wrap inside (..parenthesis..)")
//    # TODO: improve this error
//    testError(b"@not (@array)",
//              SemanticError, "unhandled Bool",
//              CommandWithNoArgumentsError, "got a command with no arguments")
//    testBoolExpression(b"@not @false", True)
//
//    testSemanticError(b"@if", "@if requires at least 1 argument")
//    testSemanticError(b"@if @false", "need more '@end'")
//    testSemanticError(b"@if @true", "need more '@end'")
//    testCommand(b"@if @false\n@end", 0, b"")
//    testCommand(b"@if @true\n@end", 0, b"")
//    testCommand(b"\n".join([
//        b"@if @false",
//        b"    @echo hey",
//        b"@end",
//    ]), 0, b"")
//    testCommand(b"\n".join([
//        b"@if @true",
//        b"    @echo hey",
//        b"@end",
//    ]), 0, b"hey\n")
//
//    testSemanticError(b"@end", "too many '@end'")
//    testSemanticError(b"@assert @end", "too many '@end'")
//    testSemanticError(b"@not @end", "too many '@end'")
//
//    testExecError(b"(@array)", CommandWithNoArgumentsError, "got a command with no arguments")
//
//    testSemanticError(b"@haveprog", "@haveprog takes 1 argument but got 0")
//    testSemanticError(b"@haveprog a b", "@haveprog takes 1 argument but got 2")
//
//    #
//    # The assign '=' operator
//    #
//    testSyntaxError(b"=foo = foo", "'=' requires space separation")
//    testSyntaxError(b"foo = =foo", "'=' requires space separation")
//    testSyntaxError(b"foo= bar", "'=' requires space separation")
//    testSyntaxError(b"foo =bar", "'=' requires space separation")
//    testSyntaxError(b"foo=bar", "'=' requires space separation")
//    testSemanticError(b"=", "unexpected '='")
//    testSemanticError(b"foo =", "expected 1 argument after '=' but got 0")
//    testSemanticError(b"= foo", "unexpected '='")
//    testSemanticError(b"= = foo", "unexpected '='")
//    testSemanticError(b"foo = =", "unexpected '='")
//    testSemanticError(b"foo = bar baz", "expected 1 argument after '=' but got 2")
//    testSemanticError(b"@not = bar", "unexpected '='")
//    testSemanticError(b"@echo =", "unexpected '='")
//    testSemanticError(b"foo = @not", "expected a String, Bool or Array after '=' but got Builtin")
//    testSemanticError(b"@true = bar", "expected a String before '=' but got Bool")
//    testSemanticError(b"@missing = bar", "'@missing' is undefined")
//    testSemanticError(b"foo = @missing", "'@missing' is undefined")
//    testSemanticError(b"@not foo = bat", "unexpected '='")
//    testSemanticError(b"(foo = bar)", "assignment '=' is not allowed inside an inline command")
//    testCommand(b"foo = bar", 0, b"")
//
//    testCommand(b"(@echo foo) = bar", 0, b"")
//    testCommand(b"foo = @false", 0, b"")
//
//    testSemanticError(b"@settmp", "@settmp requires at least 3 arguments but got 0")
//    testSemanticError(b"@settmp a", "@settmp requires at least 3 arguments but got 1")
//    testSemanticError(b"@settmp a b", "@settmp requires at least 3 arguments but got 2")
//
//    testSemanticError(b"@isfile", "@isfile takes 1 argument but got 0")
//    testSemanticError(b"@isfile foo bar", "@isfile takes 1 argument but got 2")
//    testSemanticError(b"@isdir", "@isdir takes 1 argument but got 0")
//    testSemanticError(b"@isdir foo bar", "@isdir takes 1 argument but got 2")
//
//    testAssertError(b"\n".join([
//        b"foo = bar",
//        b"@assert $foo @eq bar",
//        b"@assert @not ($foo @eq bar)",
//    ]))
//
//    testCommand(b"@echo hello", 0, b"hello\n")
//    testCommand(b"@assert (@exitcode @echo hello)", 0, b"hello\n")
//    # todo: test the same things but with a real external program, I could use the stitch intepreter itself...
//    testCommand(b"@assert (@exitcode @echo hello)", 0, b"hello\n")
//    #testCommand(b"@assert ("" @eq (@stderr @echo hello))", 0, b"hello")
//    #testCommand(b"@assert ("" @eq (@stderr @echo hello)", b"hello")
//
//
//    testSemanticError(b"@setenv", "@setenv takes 2 arguments but got 0")
//    testSemanticError(b"@setenv FOO", "@setenv takes 2 arguments but got 1")
//    testSemanticError(b"@setenv FOO BAR BAZ", "@setenv takes 2 arguments but got 3")
//    testSemanticError(b"@setenv @false BAR", "@setenv requires 2 Strings but got Bool")
//    testSemanticError(b"@setenv FOO @false", "@setenv requires 2 Strings but got Bool")
//    testCommand(b"@setenv FOO BAR", 0, b"")
//    # TODO: remove this if/when environment variables are no longer "sticky" across multiple ScriptContexts
//    testCommand(b"@unsetenv FOO", 0, b"")
//
//    testSemanticError(b"@env", "@env takes 1 argument but got 0")
//    testSemanticError(b"@env PWD PWD", "@env takes 1 argument but got 2")
//    testSemanticError(b"@env @false", "@env requires a String but got Bool")
//    testExecError(b"@env THIS_IS_NOT_DEFINED", prototype.UndefinedEnvironmentVariableError,
//                  "undefined environment variable 'THIS_IS_NOT_DEFINED'")
//
//    testSemanticError(b"@envdefault", "@envdefault takes 2 arguments but got 0")
//    testSemanticError(b"@envdefault foo", "@envdefault takes 2 arguments but got 1")
//    testSemanticError(b"@envdefault foo bar baz", "@envdefault takes 2 arguments but got 3")
//    testSemanticError(b"@envdefault @false foo", "@envdefault requires a String for its first argument but got Bool")
//    testSemanticError(b"@envdefault foo @false", "@envdefault requires a String for its second argument but got Bool")
//    testCommand(b"@envdefault THIS_IS_NOT_DEFINED foo", 0, b"foo")
//
//    # test lazy default semantics
//    testSemanticError(b"@echo (@envdefault anything @a_semantic_error)", "'@a_semantic_error' is undefined")
//    testSemanticError(b"@echo (@envdefault anything $does_not_exist)", "'$does_not_exist' is undefined")
//    testCommand(b"\n".join([
//        b"@setenv FOO bar",
//        b"@echo (@envdefault FOO $does_not_exist)",
//        b"@unsetenv FOO",
//    ]), 0, b"bar\n")
//
//    testSemanticError(b"@unsetenv", "@unsetenv takes 1 argument but got 0")
//    testSemanticError(b"@unsetenv PWD PWD", "@unsetenv takes 1 argument but got 2")
//    testSemanticError(b"@unsetenv @false", "@unsetenv requires a String but got Bool")
//    testExecError(b"@unsetenv THIS_IS_NOT_DEFINED", UndefinedEnvironmentVariableError,
//                  "undefined environment variable 'THIS_IS_NOT_DEFINED'")
//
//    testExecError(b"@unsetenv FOO", UndefinedEnvironmentVariableError,
//                  "undefined environment variable 'FOO'")
//    testAssertError(b"\n".join([
//        b"@setenv FOO bar",
//        b"@assert (@env FOO) @eq bar",
//        b"@assert @not ((@env FOO) @eq bar)",
//    ]))
//    # cleanup since environment variables are global right now
//    testCommand(b"@unsetenv FOO", 0, b"")
//    testCommand(b"\n".join([
//        b"@setenv FOO bar",
//        b"@assert (@env FOO) @eq bar",
//        # cleanup since environment variables are global right now
//        b"@unsetenv FOO",
//        b"@assert (@envdefault FOO bar) @eq bar",
//    ]), 0, b"")
//
//    testSemanticError(b"@call", "@call requires at least one argument")
//    testSemanticError(b"@call @false", "@call requires Strings but got Bool")
//    testExecError(b"@call this-file-does-not-exist", MissingStitchScriptError,
//                  "stitch script 'this-file-does-not-exist' does not exist")
//
//    testSemanticError(b"@cat a b", "'@cat' takes 0 or 1 arguments but got 2")
//    testCommand(b"@cat", 0, b"what", Options(stdin=b"what"))
//
//    #
//    # Arrays
//    #
//    testSemanticError(b"@array @false", "@array requires Strings but got Bool")
//    testSemanticError(b"@array @echo", "@array requires Strings but got Builtin '@echo'")
//    testArrayExpression(b"@array", prototype.Array([]))
//    testArrayExpression(b"@array a", prototype.Array(["a"]))
//    testArrayExpression(b"@array a b", prototype.Array(["a", "b"]))
//    testArrayExpression(b"@array a b car", prototype.Array(["a", "b", "car"]))
//    testCommand(b"a = (@array)", 0, b"")
//    testCommand(b"a = (@array a)", 0, b"")
//    testCommand(b"a = (@array a b c)", 0, b"")
//    testCommand(b"@echo (@array)", 0, b"")
//    testCommand(b"@echo (@array a b c)", 0, b"a b c\n")
//    testCommand(b'@assert "" @eq (@echo (@array))', 0, b"")
//    testCommand(b'@assert "a b c" @eq (@echo (@array a b c))', 0, b"")
//    testCommand(b"@echo foo(@array bar baz)buz", 0, b"foobarbazbuz\n")
//
//    testSemanticError(b"@len", "@len takes 1 argument but got 0")
//    testSemanticError(b"@len a", "@len requires an Array but got 'String'")
//    testSemanticError(b"@len @array", "@len requires an Array but got 'Builtin'")
//    testCommand(b"@len (@array)", 0, b"0")
//    testCommand(b"@len (@array a)", 0, b"1")
//    testCommand(b"a = (@array)\n@len $a", 0, b"0")
//    testCommand(b"a = (@array a b c)\n@len $a", 0, b"3")
//    testCommand(b"a = (@array a b c)\n@assert (@len $a) @eq 3", 0, b"")
//    testCommand(b"a = (@array a b c)\n@assert (@len (@array 1 2 3 4)) @eq 4", 0, b"")
//
//    testSemanticError(b"@index", "@index takes 2 arguments but got 0")
//    testSemanticError(b"@index a", "@index takes 2 arguments but got 1")
//    testSemanticError(b"@index a b c", "@index takes 2 arguments but got 3")
//    testSemanticError(b"@index a 0", "@index requires an Array for argument 1 but got 'String'")
//    testSemanticError(b"@index (@array) @true", "@index requires a number String for argument 2 but got 'Bool'")
//    testSemanticError(b"@index (@array) a", "@index requires a number for argument 2 but got 'a'")
//    testSemanticError(b"@index (@array) -1", "@index -1 cannot be negative")
//    testSemanticError(b"@index (@array) 0", "@index 0 is out of bounds (length=0)")
//    testSemanticError(b"@index (@array a) 4", "@index 4 is out of bounds (length=1)")
//    testCommand(b"@index (@array a) 0", 0, b"a")
//    testCommand(b"@index (@array a b c d e f g) 3", 0, b"d")
//
//    #
//    # Pipes
//    #
//    testSyntaxError(b"0@pipe", "'@pipe' requires space separation")
//    testSemanticError(b"@pipe", "unexpected '@pipe'")
//    def testBadPipeNode(node_str, after_only):
//        error = "'@pipe' requires an inline command but got '{}'".format(node_str)
//        if not after_only:
//            testSemanticError("{} @pipe (@echo)".format(node_str).encode('utf8'), error)
//        testSemanticError("(@echo) @pipe {}".format(node_str).encode('utf8'), error)
//
//    testBadPipeNode("a", after_only=False)
//    testBadPipeNode("$foo", after_only=False)
//    testBadPipeNode("@false", after_only=False)
//    testBadPipeNode("@or", after_only=False)
//    testBadPipeNode("@not", after_only=True)
//
//    testSemanticError(b"@not @pipe b", "unexpected '@pipe'")
//    testSemanticError(b"@not (@echo) @pipe (@echo)", "@not expects a Bool but got an ExitCode")
//    testSemanticError(b"(@echo) @pipe (@echo) @false", "expected '@pipe' but got '@false'")
//    testSemanticError(b"(@true) @pipe (@echo)", "unexpected Bool within @pipe expression")
//    testSemanticError(b"(@echo) @pipe (@true)", "unexpected Bool within @pipe expression")
//    testSemanticError(b"(@true @and @true) @pipe (@echo)", "unexpected binary expression within @pipe expression")
//    testError(b"((@array)) @pipe (@echo)", CommandWithNoArgumentsError, "got a command with no arguments")
//    testError(b"(@echo) @pipe ((@array))", CommandWithNoArgumentsError, "got a command with no arguments")
//
//    testCommand(b"(@echo hello) @pipe (@cat)", 0, b"hello\n")
//
//    testSemanticError(b"(@exitcode (@echo hello) @pipe (@cat))", "unhandled Bool")
//    testCommand(b"@assert (@exitcode (@echo hello) @pipe (@cat))", 0, b"hello\n")
//    testSemanticError(b"a @pipe b", "'@pipe' requires an inline command but got 'a'")
//    testCommand(b"(@echo hello) @pipe (@echo pipes)", 0, b"pipes\n")
//    testCommand(b"@assert (@exitcode (@echo) @pipe (@echo))", 0, b"")
//
//    testCommand(b"(@assert (@exitcode @echo)) @pipe (@cat)", 0, b"")
//    testSemanticError(b"(@isfile this_does_not_exist) @pipe (@cat)", "commands with @isfile cannot be piped")
//    testSemanticError(b"(@exitcode @echo hello) @pipe (@cat)", "commands with @exitcode cannot be piped")
//    testSemanticError(b"(@echo hello) @pipe (@exitcode @false)", "unhandled Bool")
//    testSemanticError(b"(@echo hello) @pipe (@exitcode @true @and @true)",
//                      "@exitcode is not compatible with binary expressions")
//    testError(b"(@echo hello) @pipe (@exitcode this_program_does_not_exist)",
//              SemanticError, "unhandled Bool",
//              MissingProgramError, "unable to find program 'this_program_does_not_exist' in PATH")
//    testError(b"(@echo hello) @pipe (@exitcode (@array))",
//              SemanticError, "unhandled Bool",
//              CommandWithNoArgumentsError, "got a command with no arguments")
//    testSemanticError(b"(@echo hello) @pipe (@exitcode @cat)", "unhandled Bool whose value is True")
//    testCommand(b"@assert (@echo hello) @pipe (@exitcode @cat)", 0, b"hello\n")
//    testSemanticError(b"@assert ((@echo hello) @pipe (@exitcode @cat))",
//                      "this inline command captured multiple objects, a Bool and stdout")
//    # TODO: implement captureobj and add this test
//    #       or maybe look into the last command of a pipe chain and limit it somehow?
//    #testCommand(b"@assert (@captureobj (@echo hello) @pipe (@exitcode @cat))", 0, b"hello\n")
//    testCommand(b"@assert (@exitcode (@echo hello) @pipe (@cat))", 0, b"hello\n")
//
//    testCommand(b"cat @callerworkdir/assetsfortest/hellomsg", 0, b"Hello Stitch!\n")
//    testCommand(b"(@echo hello) @pipe (cat)", 0, b"hello\n")
//    testCommand(b"(@echo hello) @pipe (@cat) @pipe (@cat)", 0, b"hello\n")
//    testCommand(b"(@echo hello) @pipe (@cat) @pipe (cat)", 0, b"hello\n")
//    testCommand(b"(@echo hello) @pipe (cat) @pipe (@cat)", 0, b"hello\n")
//    testCommand(b"(@echo hello) @pipe (cat) @pipe (cat)", 0, b"hello\n")
//    testCommand(b"(@echo hello) @pipe (cat) @pipe (cat) @pipe (@cat) @pipe (cat) @pipe (@cat)", 0, b"hello\n")
//
//    testExecError(b"@unreachable", UnreachableError, "reached @unreachable")
//
//    if os.name == "nt":
//        testSemanticError(b"@getuid", "@getuid not supported on Windows")
//    else:
//        testSemanticError(b"@getuid a", "@getuid takes 0 arguments but got 1")
//        testCommand(b"@getuid", 0, str(os.getuid()).encode('ascii'))
//
//    testSemanticError(b"@exit", "@exit takes 1 argument but got 0")
//    testSemanticError(b"@exit foo", "@exit requires an integer but got 'foo'")
//    # TODO: this should be an error because @exit returns NoReturn
//    #testSemanticError(b"@exit 0\n@echo foo", "")
//
//    testSemanticError(b"@stdin2file", "@stdin2file takes 1 argument but got 0")
//    testSemanticError(b"@stdin2file a b", "@stdin2file takes 1 argument but got 2")
//
//    def verifyAndRemoveFile(filename, content: bytes):
//        with open(filename, "rb") as file:
//            expectMatch(file.read(), content)
//        os.remove(filename)
//
//    testfile = os.path.join(outdir, "testfile")
//    testCommand(b"@stdin2file @callerworkdir/out/testfile", 0, b"", Options(stdin=b"what"))
//    verifyAndRemoveFile(testfile, b"what")
//    testCommand(b"(@echo another) @pipe (@stdin2file @callerworkdir/out/testfile)", 0, b"")
//    verifyAndRemoveFile(testfile, b"another\n")
//
//    testCommand(b"@stdin2file @callerworkdir/this-dir-does-not-exist/foo", 1, b"", Options(stdin=b"what"))
//
//    testSemanticError(b"foo = (uname)\n$foo = baz", "variable names must be known at verification time")
//
//    runStitchTests()
//
//    print("test: success")
//
}
