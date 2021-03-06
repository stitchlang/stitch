from enum import Enum
from typing import List, Dict, Set, Union, Tuple, Optional, Pattern
import re

import tokens
import cish

class TokenKind(Enum):
    INLINE_WHITESPACE = 0
    BUILTIN_ID = 1
    USER_ID = 2
    ARG = 3
    NEWLINE = 4
    QUOTED_STRING = 5
    COMMENT = 6
    OPEN_PAREN = 7
    CLOSE_PAREN = 8
    DELIMITED_STRING = 9
    ESCAPE_SEQUENCE = 10

class Pattern:
    def __init__(self, kind: TokenKind, re_string: str):
        self.kind = kind
        self.re = re.compile("^" + re_string)

PATTERNS = []
with open(tokens.getTokensTxtFilename(), "r") as tokens_file:
    for line in tokens_file:
        name, pattern = tokens.parseLine(line)
        #print("{:20} {}".format(name, pattern))
        if name.startswith("DELIMITED_STRING_"):
            kind = TokenKind.DELIMITED_STRING
        else:
            kind = getattr(TokenKind, name)
        PATTERNS.append(Pattern(kind, pattern))

def countLinesAndColumns(s: str):
    line = 1
    column = 1
    for c in s:
        if c == "\n":
            line += 1
            column = 1
        else:
            column += 1
    return line, column

class SyntaxError(Exception):
    def __init__(self, src_prefix: str, message: str):
        super().__init__(message)
        self.src_prefix = src_prefix

# TODO: this can be implemented better
#       the string returned should never exceed max_len, so the [..snip..]
#       should cut off the actual characters of s as well
def preview(s, max_len):
    newline = s.find("\n")
    cutoff = len(s) if (newline == -1) else newline - 1
    if cutoff > max_len:
        return s[:max_len] + "[..snip..]"
    return s[:cutoff]

# this functions mirrors the one in lex.c
def lex(text: str, pattern_index_ref: cish.Ref[int]) -> int:
    for i, pattern in enumerate(PATTERNS[pattern_index_ref.value:], start=pattern_index_ref.value):
        match = pattern.re.match(text)
        if match:
            match_length = len(match.group())
            assert(match_length > 0)
            pattern_index_ref.value = i
            return match_length
    return 0

def afterLexVerifyOnlyOneMatch(text: str, pattern_index: int) -> None:
    other_pattern_index_obj = cish.Ref(pattern_index + 1)
    match_length = lex(text, other_pattern_index_obj)
    if match_length > 0:
        raise Exception("TokenPatternProblem: both patterns '{}' and '{}' matched a string starting with '{}'".format(
            PATTERNS[pattern_index].kind, PATTERNS[other_pattern_index_obj.value].kind, text[:match_length]))

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
def scan(src: str, pos: int, verify_one_match: bool = True) -> Optional[Tuple[Pattern,int]]:
    if pos == len(src):
        return None
    next = src[pos:]

    match_pattern_index_obj = cish.Ref(0)
    match_length = lex(next, match_pattern_index_obj)
    if match_length > 0:
        if verify_one_match:
            afterLexVerifyOnlyOneMatch(next, match_pattern_index_obj.value)
        return (PATTERNS[match_pattern_index_obj.value], match_length)

    # time to try to figure out what went wrong
    src_prefix = src[:pos]
    c = next[0]
    if c == '"':
        raise SyntaxError(src_prefix, "missing close quote for: {}".format(preview(next, 30)))
    
    # I think we need at most 2 characters to see what went wrong
    bad_str = next[:min(len(next), 2)]
    raise SyntaxError(src_prefix, "unrecognized character sequence '{}'".format(bad_str))
