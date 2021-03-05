#
# TODO: at some point I should probably use a grammar to describe this logic
#
# Also, I should probably include binary operatos in the parse tree instead of using semantic
# analysis to find them (SyntaxErrors better than SemanticErrors better than RuntimeErrors).
#

from typing import List, Dict, Set, Union, Tuple, Optional

import tokens
import lex

class Node:
    def __init__(self, src):
        self.src = src
class NodeToken(Node):
    def __init__(self, src, s):
        Node.__init__(self, src)
        self.s = s
    def __repr__(self):
        return "Token({})".format(self.s)
class NodeVariable(Node):
    def __init__(self, src, id, is_at):
        Node.__init__(self, src)
        self.id = id
        self.is_at = is_at
    def __repr__(self):
        return "Variable({}{})".format("@" if self.is_at else "$", self.id)
class NodeInlineCommand(Node):
    def __init__(self, src, nodes):
        Node.__init__(self, src)
        self.nodes = nodes
    def __repr__(self):
        return "InlineCommand({})".format(", ".join([str(n) for n in self.nodes]))
class NodeMultiple(Node):
    def __init__(self, src, nodes):
        Node.__init__(self, src)
        self.nodes = nodes
    def __repr__(self):
        return "Multiple({})".format(", ".join([str(n) for n in self.nodes]))

def combineNodes(existing_node: Optional[Node], new_node: Node) -> Node:
    if not existing_node:
        return new_node
    if isinstance(existing_node, NodeMultiple):
        existing_node.nodes.append(new_node)
        return existing_node
    return NodeMultiple(existing_node.src + new_node.src, [existing_node, new_node])

class ParseToken:
    def __init__(self, pos: int, pattern: lex.Pattern, length: int):
        self.pos = pos
        self.pattern = pattern
        self.length = length
    # returns True on EOF
    def skipInlineWhitespace(self, src: str) -> bool:
        if self.pattern.kind == lex.TokenKind.INLINE_WHITESPACE:
            token_start = self.pos + self.length
            lex_token = lex.scan(src, token_start)
            if not lex_token:
                return True
            self.pattern, self.length = lex_token
            assert(self.length > 0)
            # should be impossible to get 2 inline whitespace tokens in a row
            assert(self.pattern.kind != lex.TokenKind.INLINE_WHITESPACE)
            self.pos = token_start
        return False


def parseNode(src: str, token: ParseToken) -> Tuple[Node, Optional[ParseToken]]:
    assert(token.pos < len(src))
    assert(token.pattern.kind != lex.TokenKind.INLINE_WHITESPACE)
    assert(token.pattern.kind != lex.TokenKind.COMMENT)
    assert(token.pattern.kind != lex.TokenKind.NEWLINE)
    assert(token.pattern.kind != lex.TokenKind.CLOSE_PAREN)

    node: Optional[Node] = None
    while True:
        token_end = token.pos + token.length
        token_src = src[token.pos:token_end]
        if token.pattern.kind == lex.TokenKind.BUILTIN_ID:
            node = combineNodes(node, NodeVariable(token_src, token_src[1:].rstrip('@'), is_at=True))
            next_token_start = token_end
        elif token.pattern.kind == lex.TokenKind.USER_ID:
            node = combineNodes(node, NodeVariable(token_src, token_src[1:].rstrip("$"), is_at=False))
            next_token_start = token_end
        elif token.pattern.kind == lex.TokenKind.ARG:
            node = combineNodes(node, NodeToken(token_src, token_src))
            next_token_start = token_end
        elif token.pattern.kind == lex.TokenKind.QUOTED_STRING:
            node = combineNodes(node, NodeToken(token_src, token_src[1:-1]))
            next_token_start = token_end
        elif token.pattern.kind == lex.TokenKind.OPEN_PAREN:
            inline_cmd_nodes, right_paren_pos = parseCommand(src, token_end)
            if right_paren_pos == len(src) or ord(src[right_paren_pos]) != ord(")"):
                raise lex.SyntaxError(src[:token.pos], "missing close paren for: {}".format(lex.preview(src[token.pos:], 30)))
            node = combineNodes(node, NodeInlineCommand(src[token.pos:right_paren_pos+1], inline_cmd_nodes))
            next_token_start = right_paren_pos + 1
        elif token.pattern.kind == lex.TokenKind.DELIMITED_STRING:
            node = combineNodes(node, NodeToken(token_src, token_src[3:-1]))
            next_token_start = token_end
        elif token.pattern.kind == lex.TokenKind.ESCAPE_SEQUENCE:
            node = combineNodes(node, NodeToken(token_src, token_src[1:]))
            next_token_start = token_end
        else:
            raise Exception("codebug: unhandled token kind {}".format(token.pattern.kind))

        lex_token = lex.scan(src, next_token_start)
        if not lex_token:
            return node, None
        pattern, token_len = lex_token
        assert(token_len > 0)
        token = ParseToken(next_token_start, pattern, token_len)
        if (pattern.kind == lex.TokenKind.INLINE_WHITESPACE or
            pattern.kind == lex.TokenKind.COMMENT or
            pattern.kind == lex.TokenKind.NEWLINE or
            pattern.kind == lex.TokenKind.CLOSE_PAREN):
            return node, token

def parseCommand(src, cmd_start) -> Tuple[List[Node],int]:
    nodes: List[Node] = []
    lex_token = lex.scan(src, cmd_start)
    if not lex_token:
        return nodes, cmd_start
    first_pattern, first_token_len = lex_token
    assert(first_token_len > 0)
    token = ParseToken(cmd_start, first_pattern, first_token_len)
    if token.skipInlineWhitespace(src):
        return nodes, token.pos
    while True:
        if token.pattern.kind == lex.TokenKind.COMMENT or token.pattern.kind == lex.TokenKind.NEWLINE:
            return nodes, token.pos + token.length
        if token.pattern.kind == lex.TokenKind.CLOSE_PAREN:
            return nodes, token.pos
        node, next_token = parseNode(src, token)
        nodes.append(node)
        if not next_token:
            return nodes, len(src)
        token = next_token
        if token.pattern.kind == lex.TokenKind.INLINE_WHITESPACE:
            if token.skipInlineWhitespace(src):
                return nodes, token.pos
        else:
            assert(token.pattern.kind == lex.TokenKind.COMMENT or
                   token.pattern.kind == lex.TokenKind.NEWLINE or
                   token.pattern.kind == lex.TokenKind.CLOSE_PAREN)
