const std = @import("std");
const testing = std.testing;

pub const TokenKind = enum {
    inline_whitespace,
    builtin_id,
    user_id,
    arg,
    newline,
    assign_op,
    double_quoted_string,
    comment,
    open_paren,
    close_paren,
    single_quoted_string,
    escape_sequence,
};
const token_count = @typeInfo(TokenKind).Enum.fields.len;

pub fn lexInlineWhitespace(text: [*]const u8, limit: [*]const u8) [*]const u8 {
    std.debug.assert(@ptrToInt(limit) > @ptrToInt(text));
    var next = text;
    while (true) {
        if (next[0] != ' ' and next[0] != '\t')
            return next;
        next += 1;
        if (next == limit)
            return next;
    }
}

// [a-zA-Z0-9_\.]
fn isIdChar(c: u8) bool {
    if (c >= 'a') return c <= 'z';
    if (c >= 'A') return c <= 'Z' or c == '_';
    if (c >= '0') return c <= '9';
    return c == '.';
}

// BUILTIN_ID:   @[a-zA-Z0-9_\.]+@?
fn lexBuiltinId(text: [*]const u8, limit: [*]const u8) [*]const u8 {
    return lexId(text, limit, '@');
}
// USER_ID:     \$[a-zA-Z0-9_\.]+\$?
fn lexUserId(text: [*]const u8, limit: [*]const u8) [*]const u8 {
    return lexId(text, limit, '$');
}
fn lexId(text: [*]const u8, limit: [*]const u8, delimiter: u8) [*]const u8 {
    std.debug.assert(@ptrToInt(limit) > @ptrToInt(text));
    if (text[0] != delimiter)
        return text;
    var next = text + 1;
    if (next == limit or !isIdChar(next[0]))
        return text;
    while (true) {
        next += 1;
        if (next == limit)
            return next;
        const c = next[0];
        if (c == delimiter)
            return next + 1;
        if (!isIdChar(c))
            return next;
    }
}

//ARG:   [^ \t\n#()@$"=']+
fn lexArg(text: [*]const u8, limit: [*]const u8) [*]const u8 {
    std.debug.assert(@ptrToInt(limit) > @ptrToInt(text));
    var next = text;
    while (true) {
        const c = next[0];
        if (
               c == '\''
            or c == '@'
            or c == '='
            or c == ')'
            or c == '('
            or c == '$'
            or c == '#'
            or c == '"'
            or c == ' '
            or c == '\t'
            or c == '\n'
        )
            return next;
        next += 1;
        if (next == limit)
            return next;
    }
}

fn lexOneChar(text: [*]const u8, limit: [*]const u8, c: u8) [*]const u8 {
    std.debug.assert(@ptrToInt(limit) > @ptrToInt(text));
    return text + @as(u1, if (text[0] == c) 1 else 0);
}

// DOUBLE_QUOTED_STRING    "[^"\n]*"
fn lexDoubleQuotedString(text: [*]const u8, limit: [*]const u8) [*]const u8 {
    std.debug.assert(@ptrToInt(limit) > @ptrToInt(text));
    if (text[0] != '"')
        return text;
    var next = text + 1;
    while (true) : (next += 1) {
        if (next == limit)
            return text;
        const c = next[0];
        if (c == '"')
            return next + 1;
        if (c == '\n')
            return text;
    }
}

// COMMENT:  #[^\n]*
fn lexComment(text: [*]const u8, limit: [*]const u8) [*]const u8 {
    std.debug.assert(@ptrToInt(limit) > @ptrToInt(text));
    if (text[0] != '#')
        return text;
    var next = text + 1;
    while (true) : (next += 1) {
        if (next == limit)
            return next;
        if (next[0] == '\n')
            return next + 1;
    }
}

// SINGLE_QUOTED_STRING1   '[^'\n]+'
// SINGLE_QUOTED_STRING2   ''[^'\n][^\n]*?'''*
// SINGLE_QUOTED_STRING3   '''[^'].*?''''*
// SINGLE_QUOTED_STRING4   ''''[^'].*?'''''*
// SINGLE_QUOTED_STRING5   '''''[^'].*?''''''*
// SINGLE_QUOTED_STRING6   ''''''[^'].*?'''''''*
fn lexSingleQuotedString(text: [*]const u8, limit: [*]const u8) [*]const u8 {
    std.debug.assert(@ptrToInt(limit) > @ptrToInt(text));
    if (text[0] != '\'')
        return text;

    var next = text + 1;
    while (true) : (next += 1) {
        if (next == limit)
            return text;
        if (next[0] != '\'')
            break;
    }

    const count = @ptrToInt(next) - @ptrToInt(text);
    if (count <= 2) {
        if (next[0] == '\n')
            return text;
    }

    next += 1;
    while (true) {
        if (next == limit)
            return text;
        if (next[0] == '\'') {
            var end_count: usize = 1;
            while (true) : (end_count += 1) {
                next += 1;
                if (next == limit or next[0] != '\'') break;
            }
            if (end_count >= count)
                return next;
        } else {
            if (count <= 2 and next[0] == '\n')
                return text;
            next += 1;
        }
    }
}


// ESCAPE_SEQUENCE:    @[@#$"')(=]
fn lexEscape(text: [*]const u8, limit: [*]const u8) [*]const u8 {
    std.debug.assert(@ptrToInt(limit) > @ptrToInt(text));
    if (text[0] == '@') {
        const next = text + 1;
        if (next != limit) {
            const c = next[0];
            if (   c == '@'
                or c == '#'
                or c == '$'
                or c == '"'
                or c == '\''
                or c == '('
                or c == ')'
                or c == '='
            )
                return next + 1;
        }
    }
    return text;
}

pub fn lex(text: [*]const u8, limit: [*]const u8, inout_kind: *TokenKind) [*]const u8 {
    std.debug.assert(@ptrToInt(limit) > @ptrToInt(text));
    var i = @enumToInt(inout_kind.*);
    while (i < token_count) : (i += 1) {
        const end = switch (@intToEnum(TokenKind, i)) {
            .inline_whitespace => lexInlineWhitespace(text, limit),
            .builtin_id => lexBuiltinId(text, limit),
            .user_id => lexUserId(text, limit),
            .arg => lexArg(text, limit),
            .newline => lexOneChar(text, limit, '\n'),
            .assign_op => lexOneChar(text, limit, '='),
            .double_quoted_string => lexDoubleQuotedString(text, limit),
            .comment => lexComment(text, limit),
            .open_paren => lexOneChar(text, limit, '('),
            .close_paren => lexOneChar(text, limit, ')'),
            .single_quoted_string => lexSingleQuotedString(text, limit),
            .escape_sequence => lexEscape(text, limit),
        };
        if (@ptrToInt(end) > @ptrToInt(text)) {
            inout_kind.* = @intToEnum(TokenKind, i);
            return end;
        }
    }
    return text; // no match
}

pub fn assertNoMatchAfter(src: [*]const u8, limit: [*]const u8, kind: TokenKind) void {
    const next_kind = @enumToInt(kind) + 1;
    if (next_kind < token_count) {
        var kind_for_lex = @intToEnum(TokenKind, next_kind);
        std.debug.assert(src == lex(src, limit, &kind_for_lex));
    }
}

fn testLex(src: []const u8, expected_len: usize, expected_kind: TokenKind) !void {
    var kind: TokenKind = @intToEnum(TokenKind, 0);
    const end = lex(src.ptr, src.ptr + src.len, &kind);
    const len = @ptrToInt(end) - @ptrToInt(src.ptr);
    try testing.expectEqual(expected_len, len);

    if (len > 0) {
        try testing.expectEqual(expected_kind, kind);
        assertNoMatchAfter(src.ptr, src.ptr + src.len, kind);
    }
}

fn testId(comptime delim: []const u8, kind: TokenKind) !void {
    try testLex(delim, 0, @intToEnum(TokenKind, 0));
    try testLex(delim ++ "a", 2, kind);
    try testLex(delim ++ "a ", 2, kind);
    try testLex(delim ++ "abcd ", 5, kind);
    try testLex(delim ++ "a" ++ delim ++ "a", 3, kind);
}

test "lex basics" {
    try testLex(" ", 1, .inline_whitespace);
    try testLex("\t", 1, .inline_whitespace);
    try testLex("\t \t  \t", 6, .inline_whitespace);
    try testLex("\t \t  \ta", 6, .inline_whitespace);
    try testId("@", .builtin_id);
    try testId("$", .user_id);

    try testLex("a", 1, .arg);
    try testLex("abcd ", 4, .arg);
    try testLex("!%&`*+,-./09:;<>?^_`", 20, .arg);

    try testLex("\n", 1, .newline);
    try testLex("=", 1, .assign_op);

    try testLex("\"\"", 2, .double_quoted_string);
    try testLex("\"abcd\"", 6, .double_quoted_string);

    try testLex("#", 1, .comment);
    try testLex("#\n", 2, .comment);
    try testLex("#!\"#$%&'()*+,-./09:;<=>?@AZ[\\]^_`az{|}~\n", 40, .comment);

    try testLex("(", 1, .open_paren);
    try testLex(")", 1, .close_paren);

    try testLex("''", 0, undefined);
    try testLex("'\n'", 0, undefined);
    try testLex("''\n''", 0, undefined);
    try testLex("'''\n'''", 7, .single_quoted_string);
    try testLex("'''\n''''", 8, .single_quoted_string);
    try testLex("'a'", 3, .single_quoted_string);
    try testLex("''a''", 5, .single_quoted_string);
    try testLex("'\"hello\"'", 9, .single_quoted_string);
    try testLex("''hello 'there' \"again\"''", 25, .single_quoted_string);

    try testLex("@@", 2, .escape_sequence);
    try testLex("@#", 2, .escape_sequence);
    try testLex("@$", 2, .escape_sequence);
    try testLex("@\"", 2, .escape_sequence);
    try testLex("@'", 2, .escape_sequence);
    try testLex("@(", 2, .escape_sequence);
    try testLex("@)", 2, .escape_sequence);
    try testLex("@=", 2, .escape_sequence);
}

test "ensure all 1-char strings match single token" {
    // verify that all 1-character strings only lex as 1 kind of token
    {
        var c: u8 = 0;
        while (true) : (c += 1) {
            const buf = [1]u8 {c};
            if (c == '"' or c == '$' or c == '\'' or c == '@') {
                try testLex(&buf, 0, undefined);
            } else {
                try testLex(&buf, 1, switch (c) {
                    '\t', ' ' => .inline_whitespace,
                    '\n' => .newline,
                    '#' => .comment,
                    '(' => .open_paren,
                    ')' => .close_paren,
                    '=' => .assign_op,
                    else => .arg,
                });
            }
            if (c == 255) break;
        }
    }

    // TODO: should we go through all 2 or 3 character strings?
}
