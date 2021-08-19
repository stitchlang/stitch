const std = @import("std");
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;

const lex = @import("lex.zig");

const assert_one_match = true;

const SyntaxError = error {
    UnknownToken,
};

const RunHooks = struct {
    allocator: *std.mem.Allocator,
    onSyntaxError: fn(err: SyntaxError, pos: [*]const u8) void,
};

const LexResult = struct {
    kind: lex.TokenKind,
    end: [*]const u8,
};
fn scan(hooks: RunHooks, text: [*]const u8, limit: [*]const u8) RunFailedError!LexResult {
    std.debug.assert(@ptrToInt(text) < @ptrToInt(limit));
    var kind = lex.token0;
    const end = lex.lex(text, limit, &kind);
    if (end == text) {
        hooks.onSyntaxError(SyntaxError.UnknownToken, text);
        return error.RunFailed;
    }
    if (assert_one_match)
        lex.assertNoMatchAfter(text, limit, kind);
    return LexResult { .kind = kind, .end = end };
}

const RunFailedError = error { RunFailed } || std.mem.Allocator.Error;
pub fn runSlice(hooks: RunHooks, s: []const u8) RunFailedError![*]const u8 {
    return run(hooks, s.ptr, s.ptr + s.len);
}
pub fn run(hooks: RunHooks, start: [*]const u8, limit: [*]const u8) RunFailedError![*]const u8 {
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
    hooks: RunHooks,
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
        std.debug.print("[DEBUG-ECHO-OUTPUT] ", .{});
        if (args.items.len == 0) {
            std.debug.print("\n", .{});
        } else {
            for (args.items[0 .. args.items.len - 1]) |arg| {
                std.debug.print("{s} ", .{arg});
            }
            std.debug.print("{s}\n", .{args.items[args.items.len-1]});
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

fn allocArgStrings(hooks: RunHooks, args: *ArrayList([]const u8), start: [*]const u8, limit: [*]const u8) RunFailedError![*]const u8 {
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
                @panic("not impl");
            },
            .open_paren => {
                @panic("not impl");
            },
        }
        next = token.end;
    }
}

fn testOnSyntaxError(err: SyntaxError, pos: [*]const u8) void {
    _ = pos;
    std.debug.print("got syntax error {} (TODO: print more info based on pos)\n", .{err});
}

test {
    const hooks = RunHooks {
        .allocator = std.testing.allocator,
        .onSyntaxError = testOnSyntaxError,
    };
    _ = try runSlice(hooks, "@echo");
    _ = try runSlice(hooks, "@echo@");
    _ = try runSlice(hooks, "@echo \"hey this is a string!\"");
    _ = try runSlice(hooks, "@echo \"hey this is a string and an \"arg\" put together\"");
    _ = try runSlice(hooks, "@echo@ hello");
    _ = try runSlice(hooks, "@echo hello");
}
