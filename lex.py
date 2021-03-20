from enum import IntEnum
from typing import List, Dict, Set, Union, Tuple, Optional
import re

import tokens
from cish import Ref, StringPtr

class TokenKind(IntEnum):
    INLINE_WHITESPACE = 0
    BUILTIN_ID = 1
    USER_ID = 2
    ARG = 3
    NEWLINE = 4
    ASSIGN_OP = 5
    DOUBLE_QUOTED_STRING = 6
    COMMENT = 7
    OPEN_PAREN = 8
    CLOSE_PAREN = 9
    SINGLE_QUOTED_STRING1 = 10
    SINGLE_QUOTED_STRING2 = 11
    SINGLE_QUOTED_STRING3 = 12
    SINGLE_QUOTED_STRING4 = 13
    SINGLE_QUOTED_STRING5 = 14
    SINGLE_QUOTED_STRING6 = 15
    ESCAPE_SEQUENCE = 16

class Pattern:
    def __init__(self, kind: TokenKind, re_string: bytes):
        assert(isinstance(re_string, bytes))
        self.kind = kind
        self.re = re.compile(b"^" + re_string, re.DOTALL)

PATTERNS: List[Pattern] = []
with open(tokens.getTokensTxtFilename(), "rb") as tokens_file:
    for line in tokens_file:
        name, pattern = tokens.parseLine(line)
        #print("{:20} {}".format(name, pattern))
        kind = getattr(TokenKind, name.decode('ascii'))
        PATTERNS.append(Pattern(kind, pattern))

def countLinesAndColumns(s: bytes) -> Tuple[int,int]:
    assert(type(s) == bytes)
    line = 1
    column = 1
    for c in s:
        if c == ord("\n"):
            line += 1
            column = 1
        else:
            column += 1
    return line, column

class SyntaxError(Exception):
    def __init__(self, pos: int, message_str: str):
        assert(isinstance(message_str, str))
        super().__init__(message_str)
        self.pos = pos

# TODO: this can be implemented better
#       the string returned should never exceed max_len, so the [..snip..]
#       should cut off the actual characters of s as well
def preview(src: bytes, max_len: int) -> str:
    assert(isinstance(src, bytes))
    newline = src.find(b"\n")
    cutoff = len(src) if (newline == -1) else newline
    if cutoff > max_len:
        return (src[:max_len] + b"[..snip..]").decode('ascii')
    return src[:cutoff].decode('ascii')

def previewStringPtr(text: StringPtr, limit: StringPtr, max_len: int) -> str:
    return preview(text.toStringWithLimit(limit), max_len)

# this functions mirrors the one in lex.c
def lex(text: StringPtr, limit: StringPtr, pattern_index_ref: Ref[int]) -> int:
    text_bytes = text.toStringWithLimit(limit)
    for i, pattern in enumerate(PATTERNS[pattern_index_ref.value:], start=pattern_index_ref.value):
        match = pattern.re.match(text_bytes)
        if match:
            match_length = len(match.group())
            assert(match_length > 0)
            pattern_index_ref.value = i
            return match_length
    return 0

def afterLexVerifyOnlyOneMatch(text: StringPtr, limit: StringPtr, pattern_index: int) -> None:
    other_pattern_index_obj = Ref(pattern_index + 1)
    match_length = lex(text, limit, other_pattern_index_obj)
    if match_length > 0:
        raise Exception("TokenPatternProblem: both patterns '{}' and '{}' matched a string starting with '{}'".format(
            PATTERNS[pattern_index].kind, PATTERNS[other_pattern_index_obj.value].kind, text.toStringWithLength(match_length).decode('utf8')))

# NOTE: verify_one_match verifies that only 1 lexer pattern is matching the next string.
#       I think that maintaining this property on my lexer means that all the patterns
#       combined form a "regular language".  This means I could represent the entire
#       lexer in 1 single regular expression.  I'd like to maintain this property
#       of the lexer if I can.  At some point, I should write some logic that takes
#       all my individual patterns and combines them into 1 regular expression.
#
#       I think we take about a 10% hit to performance when verify_one_match is enabled.
#
# TODO: remove this verify_one_match argument when I know my combined lex patterns are regular
def scan(src: bytes, pos: int, verify_one_match: bool = True) -> Optional[Tuple[Pattern,int]]:
    assert(type(src) == bytes)
    if pos == len(src):
        return None
    next = StringPtr(src, pos)
    limit = StringPtr(src, len(src))

    match_pattern_index_obj = Ref(0)
    match_length = lex(next, limit, match_pattern_index_obj)
    if match_length > 0:
        if verify_one_match:
            afterLexVerifyOnlyOneMatch(next, limit, match_pattern_index_obj.value)
        return (PATTERNS[match_pattern_index_obj.value], match_length)

    # time to try to figure out what went wrong
    c = next.charAt(0)
    if c == ord('('):
        raise SyntaxError(pos, "missing close paren for: {}".format(previewStringPtr(next, limit, 30)))
    if c == ord('"'):
        raise SyntaxError(pos, "missing double-quote to close: {}".format(previewStringPtr(next, limit, 30)))
    next_str = next.toStringWithLimit(limit)
    for seq in (b"''''''", b"'''''", b"''''", b"'''", b"''", b"'"):
        if next_str.startswith(seq):
            phrase = "single-quote" if (len(seq) == 1) else "{} single-quote sequence".format(len(seq))
            raise SyntaxError(pos, "missing {} to close: {}".format(phrase, previewStringPtr(next, limit, 30)))
    
    # I think we need at most 2 characters to see what went wrong
    bad_str = next_str[:min(limit.subtract(next), 2)]
    raise SyntaxError(pos, "unrecognized character sequence '{}'".format(bad_str.decode('ascii')))
