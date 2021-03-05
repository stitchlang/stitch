from enum import Enum
import re
from typing import List, Dict, Set, Union, Tuple, Optional, Pattern

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

class TokenRule:
    def __init__(self, kind: TokenKind, pattern: str):
        self.kind = kind
        self.pattern = re.compile("^" + pattern)

RULES = [
    TokenRule(TokenKind.INLINE_WHITESPACE, r"[ \t]+"),
    # NOTE: there have to be different rules for @id and $id because they
    #       end with different optional characters '@?' and '$?'
    TokenRule(TokenKind.BUILTIN_ID, r"@[a-zA-Z0-9_\.]+@?"),
    TokenRule(TokenKind.USER_ID, r"\$[a-zA-Z0-9_\.]+\$?"),
    TokenRule(TokenKind.ARG, r'[^ \t\n#()@$"]+'),
    TokenRule(TokenKind.NEWLINE, r"\n"),
    TokenRule(TokenKind.QUOTED_STRING, r'"[^"]*"'),
    TokenRule(TokenKind.COMMENT, r"#.*"),
    TokenRule(TokenKind.OPEN_PAREN, r"\("),
    TokenRule(TokenKind.CLOSE_PAREN, r"\)"),
    TokenRule(TokenKind.ESCAPE_SEQUENCE, r'@[@#$")(]'),

    # uncomment this to intentionally break the token rules by having multiple
    # rules in the same group that match the same thing , verify this
    # produces errors
    #TokenRule(TokenKind.COMMENT, r"#.*"),

# To be able to tokenize them, I need to make a separate rule for every possible
# kind of delimited string. For now I'll just make  few:
] + [TokenRule(TokenKind.DELIMITED_STRING, r"@%{0}[^{0}]*{0}".format(re_str)) for (re_str) in (
    '"',
    "'",
    r"\|",
)]

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

# NOTE: verify_one_rule verifies that only 1 lexer rule is matching the next string.
#       I think that maintaining this property on my lexer means that all the rules
#       combined form a "regular language".  This means I could represent the entire
#       lexer in 1 single regular expression.  I'd like to maintain this property
#       of the lexer if I can.  At some point, I should write some logic that takes
#       all my individual rules and combines them into 1 regular expression.
#
#       I think we take about a 10% hit to performance when verify_one_rule is enabled.
#
# TODO: remove this verify_one_rule argument when I know my combined lex rules are regular
def scan(src: str, pos: int, verify_one_rule: bool = True) -> Optional[Tuple[TokenRule,int]]:
    if pos == len(src):
        return None
    next = src[pos:]
    result: Optional[Tuple[TokenRule,int]] = None
    for rule in RULES:
        match = rule.pattern.match(next)
        if match:
            match_str = match.group()
            assert(len(match_str) > 0)
            if result:
                other_rule, other_match_len = result
                min_match_len = min(len(match_str), other_match_len)
                raise Exception("TokenRuleProblem: both patterns '{}' and '{}' matched a string starting with '{}'".format(
                    rule.kind, other_rule.kind, src[pos:pos+min_match_len]))
            result = (rule, len(match_str))
            if not verify_one_rule:
                return result
    if result:
        return result

    # time to try to figure out what went wrong
    src_prefix = src[:pos]
    c = next[0]
    if c == '"':
        raise SyntaxError(src_prefix, "missing close quote for: {}".format(preview(next, 30)))
    
    # I think we need at most 2 characters to see what went wrong
    bad_str = next[:min(len(next), 2)]
    raise SyntaxError(src_prefix, "unrecognized character sequence '{}'".format(bad_str))
