#
# TODO: at some point I should probably use a grammar to describe this logic
#
# Also, I should probably include binary operatos in the parse tree instead of using semantic
# analysis to find them (SyntaxErrors better than SemanticErrors better than RuntimeErrors).
#
from enum import Enum
from typing import List, Dict, Set, Union, Tuple, Optional

import tokens
from lex import TokenKind, Token, SyntaxError
import lex

class Node:
    def __init__(self, pos: int, end: int):
        self.pos = pos
        self.end = end
class NodeToken(Node):
    def __init__(self, pos: int, end: int, s: bytes):
        assert(isinstance(s, bytes))
        Node.__init__(self, pos, end)
        self.s = s
    def __repr__(self):
        return "Token({})".format(self.s)
class NodeVariable(Node):
    def __init__(self, pos: int, end: int, id: bytes, is_at: bool):
        assert(isinstance(id, bytes))
        Node.__init__(self, pos, end)
        self.id = id
        self.is_at = is_at
    def __repr__(self):
        return "Variable({}{})".format("@" if self.is_at else "$", self.id)
class NodeInlineCommand(Node):
    def __init__(self, pos: int, end: int, nodes: List[Node]):
        Node.__init__(self, pos, end)
        self.nodes = nodes
    def __repr__(self):
        return "InlineCommand({})".format(", ".join([str(n) for n in self.nodes]))
class NodeMultiple(Node):
    def __init__(self, pos: int, end: int, nodes: List[Node]):
        Node.__init__(self, pos, end)
        self.nodes = nodes
    def __repr__(self):
        return "Multiple({})".format(", ".join([str(n) for n in self.nodes]))

class BinaryOpKind(Enum):
    ASSIGN = 0
    PIPE = 1
    OR = 2
    AND = 3
    EQ = 4
    GT = 5
    LT = 6

def binaryOpUserString(kind: BinaryOpKind):
    if kind == BinaryOpKind.ASSIGN:
        return "="
    return "@" + kind.name.lower()

class NodeBinaryOp(Node):
    def __init__(self, pos: int, end: int, kind: BinaryOpKind):
        Node.__init__(self, pos, end)
        self.kind = kind
    def __repr__(self):
        return "BinaryOp({})".format(self.kind)

binary_builtin_id_map = {
    b"pipe": BinaryOpKind.PIPE,
    b"or": BinaryOpKind.OR,
    b"and": BinaryOpKind.AND,
    b"eq": BinaryOpKind.EQ,
    b"gt": BinaryOpKind.GT,
    b"lt": BinaryOpKind.LT,
}

def parseOneNode(src: bytes, token: Token, allstringliterals: bool) -> Node:
    assert(token.pattern_kind != TokenKind.INLINE_WHITESPACE)
    assert(token.pattern_kind != TokenKind.COMMENT)
    assert(token.pattern_kind != TokenKind.NEWLINE)
    assert(token.pattern_kind != TokenKind.CLOSE_PAREN)

    if token.pattern_kind == TokenKind.BUILTIN_ID:
        id = src[token.pos+1:token.end].rstrip(b'@')
        binary_op_kind = binary_builtin_id_map.get(id)
        if binary_op_kind:
            return NodeBinaryOp(token.pos, token.end, binary_op_kind)
        return NodeVariable(token.pos, token.end, id, is_at=True)

    if token.pattern_kind == TokenKind.USER_ID:
        return NodeVariable(token.pos, token.end, src[token.pos+1:token.end].rstrip(b"$"), is_at=False)

    if token.pattern_kind == TokenKind.ARG:
        return NodeToken(token.pos, token.end, src[token.pos:token.end])

    if token.pattern_kind == TokenKind.ASSIGN_OP:
        return NodeBinaryOp(token.pos, token.end, BinaryOpKind.ASSIGN)

    if token.pattern_kind == TokenKind.DOUBLE_QUOTED_STRING:
        return NodeToken(token.pos, token.end, src[token.pos+1:token.end-1])

    if token.pattern_kind == TokenKind.OPEN_PAREN:
        inline_cmd_nodes, right_paren_pos = parseCommand(src, token.end, allstringliterals)
        if right_paren_pos == len(src) or src[right_paren_pos] != ord(")"):
            raise SyntaxError(token.pos, "missing close paren for: {}".format(lex.preview(src[token.pos:], 30)))
        return NodeInlineCommand(token.pos, right_paren_pos + 1, inline_cmd_nodes)

    if (token.pattern_kind >= TokenKind.SINGLE_QUOTED_STRING1 and
        token.pattern_kind <= TokenKind.SINGLE_QUOTED_STRING6):
        quote_count = (token.pattern_kind - TokenKind.SINGLE_QUOTED_STRING1) + 1
        data_offset = quote_count
        data = src[token.pos+data_offset:token.end-quote_count]
        if not allstringliterals:
            if not any(c in data for c in b'"\n'):
                raise SyntaxError(token.pos, "got a single-quote string literal without double-quotes nor newlines, use double quotes instead or invoke @allstringliterals")
        if quote_count >= 3 and data[0] == ord("\n"):
            data = data[1:]
        return NodeToken(token.pos, token.end, data)

    if token.pattern_kind == TokenKind.ESCAPE_SEQUENCE:
        return NodeToken(token.pos, token.end, src[token.pos+1:token.end])

    raise Exception("codebug: unhandled token kind {}".format(token.pattern_kind))

def parseNode(src: bytes, token: Token, allstringliterals: bool) -> Tuple[Node, Optional[Token]]:
    assert(token.pos < len(src))
    assert(token.pattern_kind != TokenKind.INLINE_WHITESPACE)
    assert(token.pattern_kind != TokenKind.COMMENT)
    assert(token.pattern_kind != TokenKind.NEWLINE)
    assert(token.pattern_kind != TokenKind.CLOSE_PAREN)

    node: Optional[Node] = None
    while True:
        next_node = parseOneNode(src, token, allstringliterals)
        assert(next_node.end > next_node.pos)
        if not node:
            node = next_node
        else:
            if isinstance(node, NodeMultiple):
                assert(node.nodes[-1].end == next_node.pos)
                node.nodes.append(next_node)
                node.end = next_node.end
            else:
                assert(node.end == next_node.pos)
                if isinstance(node, NodeBinaryOp):
                    raise SyntaxError(node.pos, "'{}' requires space separation".format(binaryOpUserString(node.kind)))
                node = NodeMultiple(node.pos, next_node.end, [node, next_node])
            if isinstance(next_node, NodeBinaryOp):
                raise SyntaxError(next_node.pos, "'{}' requires space separation".format(binaryOpUserString(next_node.kind)))

        scan_result = lex.scan(src, node.end)
        if not scan_result:
            return node, None
        token = Token(node.end, scan_result)
        if (token.pattern_kind == TokenKind.INLINE_WHITESPACE or
            token.pattern_kind == TokenKind.COMMENT or
            token.pattern_kind == TokenKind.NEWLINE or
            token.pattern_kind == TokenKind.CLOSE_PAREN):
            return node, token

def parseCommand(src: bytes, cmd_start: int, allstringliterals: bool) -> Tuple[List[Node],int]:
    assert(type(src) == bytes)
    nodes: List[Node] = []
    token = lex.scanSkipInlineWhitespace(src, cmd_start)
    if not token:
        return nodes, cmd_start
    while True:
        if token.pattern_kind == TokenKind.COMMENT or token.pattern_kind == TokenKind.NEWLINE:
            return nodes, token.end
        if token.pattern_kind == TokenKind.CLOSE_PAREN:
            return nodes, token.pos
        node, next_token = parseNode(src, token, allstringliterals)
        nodes.append(node)
        if not next_token:
            return nodes, len(src)
        token = next_token
        if token.pattern_kind == TokenKind.INLINE_WHITESPACE:
            scan_result = lex.scan(src, token.end)
            if not scan_result:
                return nodes, token.pos
            assert(scan_result.pattern_kind != TokenKind.INLINE_WHITESPACE)
            token = Token(token.end, scan_result)
        else:
            assert(token.pattern_kind == TokenKind.COMMENT or
                   token.pattern_kind == TokenKind.NEWLINE or
                   token.pattern_kind == TokenKind.CLOSE_PAREN)
