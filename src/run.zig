const std = @import("std");
const testing = std.testing;
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;

const lex = @import("lex.zig");

const assert_one_match = true;

const SyntaxError = error {
    UnknownToken,
};

const OutKind = enum { out, err };

const RunHooks = struct {
    allocator: *std.mem.Allocator,
    onSyntaxError: fn(base: *RunHooks, err: SyntaxError, pos: [*]const u8) void,
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
};

const LexResult = struct {
    kind: lex.TokenKind,
    end: [*]const u8,
};
fn scan(hooks: *RunHooks, text: [*]const u8, limit: [*]const u8) RunFailedError!LexResult {
    std.debug.assert(@ptrToInt(text) < @ptrToInt(limit));
    var kind = lex.token0;
    const end = lex.lex(text, limit, &kind);
    if (end == text) {
        hooks.onSyntaxError(hooks, SyntaxError.UnknownToken, text);
        return error.RunFailed;
    }
    if (assert_one_match)
        lex.assertNoMatchAfter(text, limit, kind);
    return LexResult { .kind = kind, .end = end };
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

const TestConfig = struct {
    exit: u8 = 0,
    stdout: ?[]const u8 = null,
    stderr: ?[]const u8 = null,
    stdin: ?[]const u8 = null,
};

const TestHooks = struct {
    hooks: RunHooks,
    stdout: ArrayList(u8),
    stderr: ArrayList(u8),

    pub fn init() TestHooks {
        return .{
            .hooks = .{
                .allocator = testing.allocator,
                .onWrite = onWrite,
                .onSyntaxError = onSyntaxError,
            },
            .stdout = ArrayList(u8).init(testing.allocator),
            .stderr = ArrayList(u8).init(testing.allocator),
        };
    }

    pub fn deinit(self: *TestHooks) void {
        self.stdout.deinit();
        self.stderr.deinit();
    }

    fn onWrite(base: *RunHooks, out_kind: OutKind, bytes: []const u8) RunHooks.WriteError!usize {
        const self = @fieldParentPtr(TestHooks, "hooks", base);
        const out = switch (out_kind) { .out => &self.stdout, .err => &self.stderr };
        try out.appendSlice(bytes);
        return bytes.len;
    }

    fn onSyntaxError(base: *RunHooks, err: SyntaxError, pos: [*]const u8) void {
        _ = base;
        _ = pos;
        std.debug.print("got syntax error {} (TODO: print more info based on pos)\n", .{err});
    }
};

fn runTest(src: []const u8, config: TestConfig) !void {
    var hooks = TestHooks.init();
    defer hooks.deinit();
    const end = runSlice(&hooks.hooks, src) catch |e| switch (e) {
        error.RunFailed => {
            @panic("not impl");
        },
        error.OutOfMemory => {
            @panic("not impl");
        },
    };
    _ = end;
    if (config.exit == 0) {
    }

    if (config.stdout) |stdout| {
        if (!std.mem.eql(u8, stdout, hooks.stdout.items)) {
            std.debug.print("\nError: stdout mismatch\nexpected: '{s}'\nactual  : '{s}'\n", .{stdout, hooks.stdout.items});
            return error.TestUnexpectedResult;
        }
    } else {
        try testing.expect(hooks.stdout.items.len == 0);
    }
}


test {
    try runTest("@echo", .{.stdout = "\n"});
    try runTest("@echo@", .{.stdout = "\n"});
    try runTest("@echo hello", .{.stdout = "hello\n"});
    try runTest("@echo@ hello", .{.stdout = "hello\n"});
    try runTest("@echo \"a string!\"", .{.stdout = "a string!\n"});
    try runTest("@echo \"string and \"arg\" put together\"", .{.stdout = "string and arg put together\n"});
    try runTest("@echo @@", .{.stdout = "@\n"});
    try runTest("@echo @(", .{.stdout = "(\n"});
}
