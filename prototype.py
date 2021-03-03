#!/usr/bin/env python3
import sys
import os
import subprocess
import re
from enum import Enum
from typing import List, Dict, Union, Tuple

class Node:
    def __init__(self, src):
        self.src = src
class NodeToken(Node):
    def __init__(self, src, s):
        Node.__init__(self, src)
        self.s = s
    def __repr__(self):
        return "RawString({})".format(self.s)
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
        return "NodeInlineCommand({})".format(", ".join([str(n) for n in self.nodes]))
class NodeMultiple(Node):
    def __init__(self, src, nodes):
        Node.__init__(self, src)
        self.nodes = nodes
    def __repr__(self):
        return "Multiple({})".format(", ".join([str(n) for n in self.nodes]))

def combineNodes(existing_node, new_node):
    if not existing_node:
        return new_node
    if type(existing_node) is NodeMultiple:
        existing_node.nodes.append(new_node)
        return existing_node
    return NodeMultiple(existing_node.src + new_node.src, [existing_node, new_node])

def isspaceOrd(c_ord):
    return c_ord == ord(" ")

def isIdOrd(c_ord):
    if c_ord >= ord("a"):
        return c_ord <= ord("z")
    elif c_ord >= ord("A"):
        return c_ord <= ord("Z") or c_ord == ord("_")
    elif c_ord >= ord("0"):
        return c_ord <= ord("9")
    else:
        return c_ord == ord(".")

def skipWhitespace(src, i):
    while i < len(src) and isspaceOrd(ord(src[i])):
        i += 1
    return i

# TODO: this can be implemented better
#       the string returned should never exceed max_len, so the [..snip..]
#       should cut off the actual characters of s as well
def preview(s, max_len):
    cutoff = s.find("\n")
    if cutoff == -1:
        cutoff = min(len(s), max_len)
    return s[:cutoff] + ("[..snip..]" if (len(s) > cutoff) else "")

class SyntaxError(Exception):
    def __init__(self, msg):
        self.msg = msg

def parseDelimitedString(src: str, i: int, prefix: str) -> Tuple[NodeToken, int]:
    assert(i < len(src))
    sentinel_char = src[i]
    sentinel_ord = ord(sentinel_char)
    i += 1
    start = i
    while True:
        if i == len(src):
            literal = preview(src[start-1:], 30)
            raise SyntaxError("string literal '{}{}' is missing the terminating '{}' character".format(
                prefix, literal, sentinel_char))
        if ord(src[i]) == sentinel_ord:
            s = src[start:i]
            return NodeToken(s, s), i+1
        i += 1

def parseAtOrDollarVar(src, i, is_at):
    special_char = "@" if is_at else "$"
    c_ord = ord(src[i])
    if not isIdOrd(c_ord):
        sys.exit("Error: unexpected sequence '{}{}''".format(special_char, chr(c_ord)))
    special_ord = ord(special_char)
    id_start = i
    while True:
        i += 1
        if i == len(src):
            id_end = i
            break
        c_ord = ord(src[i])
        if not isIdOrd(c_ord):
            id_end = i
            if c_ord == special_ord:
                i += 1
            break
    return NodeVariable(src[id_start-1:id_end], src[id_start:id_end], is_at=is_at), i

def parseAtExpr(src, i):
    if i == len(src):
        # TODO: add test for this error
        sys.exit("Error: got a '@' with nothing after it")
    c_ord = ord(src[i])
    if c_ord == ord("#"):
        return NodeToken("@#", "#"), i+1
    if c_ord == ord("@"):
        return NodeToken("@@", "@"), i+1
    if c_ord == ord("%"):
        if i+1 == len(src):
            sys.exit("Error: got '@%' with nothing after it")
        return parseDelimitedString(src, i+1, "@%")
    if c_ord == ord("$"):
        return NodeToken("@$", "$"), i+1
    if c_ord == ord('"'):
        return NodeToken('@"', '"'), i+1
    if c_ord == ord("("):
        return NodeToken("@(", "@"), i+1
    if c_ord == ord(")"):
        return NodeToken("@)", ")"), i+1
    return parseAtOrDollarVar(src, i, is_at=True)

def parseDollarExpr(src, i):
    if i == len(src):
        # TODO: add test for this error
        sys.exit("Error: got a '$' with nothing after it")
    c_ord = ord(src[i])
    if c_ord == ord("@"):
        return NodeToken("$@", "@"), i+1
    return parseAtOrDollarVar(src, i, is_at=False)

def parseNode(src, i):
    assert(i < len(src))
    assert(ord(src[i]) != "#")
    assert(not isspaceOrd(ord(src[i])))
    node = None
    mark = i
    while i < len(src):
        c_ord = ord(src[i])
        if c_ord == ord("@"):
            if i > mark:
                s = src[mark:i]
                node = combineNodes(node, NodeToken(s, s))
            at_node, at_str_limit = parseAtExpr(src, i+1)
            mark = at_str_limit
            i = at_str_limit
            node = combineNodes(node, at_node)
            continue
        if c_ord == ord("$"):
            if i > mark:
                s = src[mark:i]
                node = combineNodes(node, NodeToken(s, s))
            dollar_node, dollar_str_limit = parseDollarExpr(src, i+1)
            mark = dollar_str_limit
            i = dollar_str_limit
            node = combineNodes(node, dollar_node)
            continue
        if c_ord == ord('"'):
            if i > mark:
                s = src[mark:i]
                node = combineNodes(node, NodeToken(s, s))
            token_node, token_limit = parseDelimitedString(src, i, '')
            mark = token_limit
            i = token_limit
            node = combineNodes(node, token_node)
            continue
        if c_ord == ord("("):
            if i > mark:
                s = src[mark:i]
                node = combineNodes(node, NodeToken(s, s))
            inline_cmd_nodes, inline_cmd_limit = parseCommand(src, i+1)
            if inline_cmd_limit == len(src) or ord(src[inline_cmd_limit]) != ord(")"):
                # TODO: add test for this error
                sys.exit("Error: '(' is missing the closing paren ')'")
            node = combineNodes(node, NodeInlineCommand(src[i:inline_cmd_limit+1], inline_cmd_nodes))
            mark = inline_cmd_limit + 1
            i = inline_cmd_limit + 1
            continue
        if isspaceOrd(c_ord) or c_ord == ord("#") or c_ord == ord(")"):
            break
        i += 1
    if i > mark:
        s = src[mark:i]
        node = combineNodes(node, NodeToken(s, s))
    return node, i

def parseCommand(src, i):
    nodes = []
    while True:
        i = skipWhitespace(src, i)
        if i == len(src):
            break
        next_ord = ord(src[i])
        if next_ord == ord("#") or next_ord == ord(")"):
            break
        node, i = parseNode(src, i)
        if not node:
            break
        nodes.append(node)
    return nodes, i

def parseTopLevelCommand(src):
    nodes, _ = parseCommand(src, 0)
    return nodes

def stripNewline(s):
    if len(s) > 0 and ord(s[-1]) == "\n":
        if len(s) > 2 and s[-2] == "\r":
            return s[:-2]
        return s[:-1]
    return s

class StitchObject:
    pass

class Bool(StitchObject):
    def __init__(self, value):
        self.value = value
    @staticmethod
    def userTypeDescriptor():
        return "Bool"
BOOL_FALSE = Bool(False)
BOOL_TRUE = Bool(True)

class Builtin(StitchObject):
    def __init__(self, name):
        self.name = name
    def __repr__(self):
        return "@{}".format(self.name)
    @staticmethod
    def userTypeDescriptor():
        return "Builtin"
class String(StitchObject):
    def __init__(self, value):
        self.value = value
    @staticmethod
    def userTypeDescriptor():
        return "String"
class Array(StitchObject):
    def __init__(self, elements):
        self.elements = elements
    @staticmethod
    def userTypeDescriptor():
        return "Array"

class BinaryOperator(StitchObject):
    def __init__(self, name):
        self.name = name
        self.src_name = "@" + name
    def __repr__(self):
        return self.src_name
class ChainableBinaryOperator(BinaryOperator):
    def __init__(self, name):
        BinaryOperator.__init__(self, name)
class AndOperator(ChainableBinaryOperator):
    def __init__(self):
        ChainableBinaryOperator.__init__(self, "and")
    def initialValue(self, operand):
        return operandToBool(self, operand)
    def apply(self, verification_mode: bool, left: Bool, right):
        assert(verification_mode or left.value)
        right_result = operandToBool(self, right)
        if isinstance(right_result, Error) or type(right_result) == UnknownBool:
            return right_result
        return right_result
    def shortcircuit(self, result: Bool):
        return not result.value
class OrOperator(ChainableBinaryOperator):
    def __init__(self):
        ChainableBinaryOperator.__init__(self, "or")
    def initialValue(self, operand):
        return operandToBool(self, operand)
    def apply(self, verification_mode: bool, left: Bool, right):
        assert(verification_mode or not left.value)
        right_result = operandToBool(self, right)
        if isinstance(right_result, Error) or type(right_result) == UnknownBool:
            return right_result
        return right_result
    def shortcircuit(self, result: Bool):
        return result.value
class EqOperator(BinaryOperator):
    def __init__(self):
        BinaryOperator.__init__(self, "eq")
    def initialValue(self, operand):
        if type(operand) == String:
            return operand
        return opInvalidTypeError(self, operand)
    def apply(self, verification_mode: bool, left, right):
        if type(right) == String:
            return BOOL_TRUE if (left.value == right.value) else BOOL_FALSE
        return opInvalidTypeError(self, right)
class CompareOperator(BinaryOperator):
    def __init__(self, name, func):
        BinaryOperator.__init__(self, name)
        self.func = func
    def initialValue(self, operand):
        if type(operand) == String:
            # TODO: return semantic error if not a valid integer
            return int(operand.value)
        if type(operand) == UnknownString:
            return None
        return opInvalidTypeError(self, operand)
    def apply(self, verification_mode: bool, left, right):
        if left == None:
            if type(right) != String and type(right) != UnknownString:
                return opInvalidTypeError(self, right)
            return UNKNOWN_BOOL
        assert(type(left) == int)
        if type(right) == String:
            # TODO: return semantic error if not a valid integer
            right_int = int(right.value)
            return BOOL_TRUE if self.func(left, right_int) else BOOL_FALSE
        if type(right) == UnknownString:
            return UNKNOWN_BOOL
        return opInvalidTypeError(self, right)

class CommandResult(StitchObject):
    def __init__(self, exitcode: int, stdout: Union[None,str], stderr: Union[None,str]):
        self.exitcode = exitcode
        self.stdout = stdout
        self.stderr = stderr
    @staticmethod
    def userTypeDescriptor():
        return "CommandResult"
    def __repr__(self):
        return "CommandResult(exit={},stderr='{}',stdout='{}')".format(
            self.exitcode, self.stderr, self.stdout)

# an unknown value, this is used during verification
class Unknown(StitchObject):
    def __init__(self, stitch_type):
        self.stitch_type = stitch_type

class UnknownBool(Unknown):
    def __init__(self):
        Unknown.__init__(self, Bool)
    @staticmethod
    def userTypeDescriptor():
        return "Bool"
UNKNOWN_BOOL = UnknownBool()

class UnknownString(Unknown):
    def __init__(self):
        Unknown.__init__(self, String)
    @staticmethod
    def userTypeDescriptor():
        return "String"
UNKNOWN_STRING = UnknownString()

class UnknownCommandResult(Unknown):
    def __init__(self):
        Unknown.__init__(self, CommandResult)
    @staticmethod
    def userTypeDescriptor():
        return "CommandResult"
UNKNOWN_COMMAND_RESULT = UnknownCommandResult()

# TODO: not sure if the Error types will be exposed to stitch yet, or if they
#       are just an internal detail
class Error:
    def __init__(self, msg):
        self.msg = msg
class SemanticError(Error):
    def __init__(self, msg):
        Error.__init__(self, msg)
    def __repr__(self):
        return "SemanticError: {}".format(self.msg)
class AssertError(Error):
    def __init__(self, src):
        Error.__init__(self, "@assert failed")
        self.src = src
    def __repr__(self):
        return "AssertError"
class NonZeroExitCodeError(Error):
    def __init__(self, exitcode: int, stdout: Union[None,str], stderr: Union[None,str]):
        assert(exitcode != 0)
        Error.__init__(self, "command failed with exit code {}".format(exitcode))
        self.exitcode = exitcode
        self.stdout = stdout
        self.stderr = stderr
class MissingProgramError(Error):
    def __init__(self, prog):
        Error.__init__(self, "unable to find program '{}' in PATH".format(prog))
class UnexpectedMultilineError(Error):
    def __init__(self, stdout):
        Error.__init__(self, "missing '@multiline', got this multiline output\n----\n{}----\n".format(stdout))
class UndefinedEnvironmentVariableError(Error):
    def __init__(self, name):
        Error.__init__(self, "undefined environment variable '{}'".format(name))
        self.name = name

# These ExitCode and UnknownExitCode types are currently only used internally
class ExitCode:
    def __init__(self, value: int):
        self.value = value
        self.multiline = False
class UnknownExitCode:
    pass
UNKNOWN_EXIT_CODE = UnknownExitCode()

class ScriptContext:
    class Block:
        def __init__(self, enabled: Union[None,Bool]):
            self.enabled = enabled

    # TODO: add fields for logging/tracing options?
    def __init__(self, doverify, scriptfile, callerworkdir, verification_mode):
        self.doverify = doverify
        self.script_specific_builtin_objects = {
            "scriptfile": String(scriptfile),
            "scriptdir": String(os.path.dirname(scriptfile)),
            # NOTE: callerworkdir will need to be forwarded to any sub-scripts
            #       maybe I can just use an environment variable
            "callerworkdir": String(callerworkdir),
        }
        self.var_map = {}
        self.verify_started = {}
        self.verification_mode = verification_mode
        self.blockStack = [ScriptContext.Block(enabled=True)]
    def pushBlock(self, enabled: bool) -> None:
        self.blockStack.append(ScriptContext.Block(enabled))
    def popBlock(self) -> Union[None,Error]:
        self.blockStack.pop()
        if len(self.blockStack) == 0:
            return SemanticError("too many '@end'")

class DataHandler:
    pass
class StringBuilder(DataHandler):
    def __init__(self):
        self.output = ""
    def isEmpty(self):
        return len(self.output) == 0
    @staticmethod
    def descriptor():
        return "capture"
    def captured(self):
        return self.output
    def handle(self, s):
        if len(s) > 0:
            self.output += s
            if s[-1] != "\n":
                self.output += "\n"
class ConsolePrinter(DataHandler):
    def isEmpty(self):
        return True
    def descriptor():
        return "console"
    def captured(self):
        return None
    def handle(self, s):
        if len(s) > 0:
            if s[-1] == '\n':
                print(s, end='')
            else:
                print(s)
CONSOLE_PRINTER = ConsolePrinter()

class Capture:
    def __init__(self, exitcode: bool, stdout: DataHandler, stderr: DataHandler):
        self.exitcode = exitcode
        self.stdout = stdout
        self.stderr = stderr
    def __repr__(self):
        return "Capture(exitcode={},stdout={},stderr={})".format(
            self.exitcode, self.stdout.descriptor(), self.stderr.descriptor())

class CommandContext:
    def __init__(
            self,
            script: ScriptContext,
            parent: 'CommandContext',
            depth: int,
            capture: Capture,
            builtin_prefix_count: int,
            ambiguous_op: Union[None,str],
            var_map: Dict[str,StitchObject] = {}
    ):
        self.script = script
        self.parent = parent
        self.depth = depth
        self.capture = capture
        self.builtin_prefix_count = builtin_prefix_count
        # true if the current command is inside an ambiguous operator (currently just @not)
        self.ambiguous_op = ambiguous_op
        self.var_map = var_map
    def createChild(self) -> 'CommandContext':
        return type(self)(self.script, self, self.depth+1,
                          Capture(exitcode=False,stdout=StringBuilder(),stderr=self.capture.stderr),
                          builtin_prefix_count=0, ambiguous_op=None)
    def nextBuiltin(self, ambiguous_op: Union[None,str]) -> 'CommandContext':
        return type(self)(self.script, self.parent, self.depth, self.capture,
                          self.builtin_prefix_count + 1, ambiguous_op, self.var_map)

class BuiltinMethods:
    def note(cmd_ctx: CommandContext, nodes: List[Node]):
        return ExitCode(0)
    def echo(cmd_ctx: CommandContext, nodes: List[Node]):
        args = []
        error = nodesToArgs(cmd_ctx, nodes, args)
        if error:
            return error
        cmd_ctx.capture.stdout.handle(" ".join(args))
        return ExitCode(0)
    def set(cmd_ctx: CommandContext, nodes: List[Node]):
        if len(nodes) != 2:
            return SemanticError("@set expects 2 arguments but got {}".format(len(nodes)))
        is_unknown = expandSetArgs(cmd_ctx, nodes[0], nodes[1], "@set", cmd_ctx.script.var_map)
        if isinstance(is_unknown, Error):
            return is_unknown
        return UNKNOWN_EXIT_CODE if is_unknown else ExitCode(0)
    def settmp(cmd_ctx: CommandContext, nodes: List[Node]):
        if len(nodes) < 3:
            sys.exit("Error: the @settmp builtin requires at least 3 arguments")
        is_unknown = expandSetArgs(cmd_ctx, nodes[0], nodes[1], "@settmp", cmd_ctx.var_map)
        if isinstance(is_unknown, Error):
            return is_unknown
        #print("DEBUG: settmp '{}' to '{}'".format(varname, value))
        return runCommandNodes(cmd_ctx.nextBuiltin(ambiguous_op=None), nodes[2:])
    def setenv(cmd_ctx: CommandContext, nodes: List[Node]):
        if len(nodes) != 2:
            return SemanticError("@setenv expects 2 arguments but got {}".format(len(nodes)))
        name, value = expandSetArgsCommon(cmd_ctx, nodes[0], nodes[1], "@setenv")
        if isinstance(name, Error):
            return name
        if type(value) == String:
            is_unknown = False
        elif type(value) == UNKNOWN_STRING:
            is_unknown = True
        else:
            return SemanticError("@setenv requires a String for its 2nd argument but got {}".format(value.userTypeDescriptor()))
        value = value.value
        if not cmd_ctx.script.verification_mode:
            os.environ[name] = value
        return UNKNOWN_EXIT_CODE if is_unknown else ExitCode(0)
    def unsetenv(cmd_ctx: CommandContext, nodes: List[Node]):
        if len(nodes) != 1:
            return SemanticError("@unsetenv expects 1 argument but got {}".format(len(nodes)))
        name = expandNode(cmd_ctx, nodes[0])
        if isinstance(name, Error):
            return name
        if type(name) == UnknownString:
            return UNKNOWN_EXIT_CODE
        if type(name) != String:
            return SemanticError("@unsetenv requires a String but got {}".format(name.userTypeDescriptor()))
        name = name.value
        if not name in os.environ:
            return UndefinedEnvironmentVariableError(name)
        del os.environ[name]
        return ExitCode(0)
    def env(cmd_ctx: CommandContext, nodes: List[Node]):
        if len(nodes) != 1:
            return SemanticError("@env expects 1 argument but got {}".format(len(nodes)))
        name = expandNode(cmd_ctx, nodes[0])
        if isinstance(name, Error):
            return name
        if type(name) == UnknownString:
            return UNKNOWN_EXIT_CODE
        if type(name) != String:
            return SemanticError("@env requires a String but got {}".format(name.userTypeDescriptor()))
        if cmd_ctx.script.verification_mode:
            return UNKNOWN_EXIT_CODE
        name = name.value
        value = os.environ.get(name)
        if value == None:
            return UndefinedEnvironmentVariableError(name)
        cmd_ctx.capture.stdout.handle(value)
        return ExitCode(0)
    def multiline(cmd_ctx: CommandContext, nodes: List[Node]):
        if len(nodes) == 0:
            return SemanticError("@multiline requires at least 1 argument")
        if cmd_ctx.parent == None:
            return SemanticError("the @multiline builtin is only supported within an (..inline command..)")
        if cmd_ctx.capture.stdout == cmd_ctx.parent.capture.stdout:
            return SemanticError("got @multiline but stdout is not being captured?  what's going on?")
        result = runCommandNodes(cmd_ctx.nextBuiltin(ambiguous_op=None), nodes)
        if isinstance(result, Error) or type(result) == UnknownExitCode or type(result) == UnknownBool:
            return result
        if type(result) == Bool:
            return SemanticError("@multiline does not accept Bool")
        assert(type(result) == ExitCode)
        result.multiline = True
        return result
    def exitcode(cmd_ctx: CommandContext, nodes: List[Node]):
        if len(nodes) == 0:
            return SemanticError("@exitcode requires at least 1 argument")
        if cmd_ctx.parent == None:
            return SemanticError("the @exitcode builtin is only supported within an (..inline command..)")
        if cmd_ctx.capture.exitcode:
            return SemanticError("@exitcode was given more than once in the same inline command")
        cmd_ctx.capture.exitcode = True
        if cmd_ctx.capture.stdout == cmd_ctx.parent.capture.stdout:
            # I might be able to just ignore this
            sys.exit("stdout has already been 'uncaptured', maybe by another directive?")
        assert(type(cmd_ctx.capture.stdout) == StringBuilder)
        assert(len(cmd_ctx.capture.stdout.output) == 0)
        cmd_ctx.capture.stdout = cmd_ctx.parent.capture.stdout
        return runCommandNodes(cmd_ctx.nextBuiltin(ambiguous_op=None), nodes)
    def not_(cmd_ctx: CommandContext, nodes: List[Node]):
        result = expandNodesToBool(cmd_ctx.nextBuiltin(ambiguous_op="@not"), nodes, "@not", True)
        if isinstance(result, Error) or isinstance(result, UnknownBool):
            return result
        assert(type(result) == Bool)
        return BOOL_FALSE if result.value else BOOL_TRUE
    def isfile(cmd_ctx: CommandContext, nodes: List[Node]):
        error = disableIncompatibleCapture(cmd_ctx, "@isfile")
        if error:
            return error
        s = expandOneNodeToString(cmd_ctx, nodes, "@isfile")
        if isinstance(s, Error):
            return s
        if type(s) == String:
            return BOOL_TRUE if os.path.isfile(s.value) else BOOL_FALSE
        assert(type(s) == UnknownString)
        assert(cmd_ctx.script.verification_mode)
        return UNKNOWN_BOOL
    def assert_(cmd_ctx: CommandContext, nodes: List[Node]):
        result = expandNodesToBool(cmd_ctx.nextBuiltin(ambiguous_op=None), nodes, "@assert", False)
        if isinstance(result, Error):
            return result
        if type(result) == UnknownBool:
            return UNKNOWN_EXIT_CODE
        assert(type(result) == Bool)
        if not result.value:
            return AssertError(" ".join([n.src for n in nodes]))
        return ExitCode(0)
    def if_(cmd_ctx: CommandContext, nodes: List[Node]):
        result = expandNodesToBool(cmd_ctx.nextBuiltin(ambiguous_op=None), nodes, "@if", True)
        if isinstance(result, Error):
            return result
        if type(result) == Bool:
            cmd_ctx.script.pushBlock(enabled=result.value)
            return ExitCode(0)
        assert(cmd_ctx.script.verification_mode and (
            type(result) == UnknownBool or
            type(result) == UnknownCommandResult))
        cmd_ctx.script.pushBlock(enabled=True)
        return UNKNOWN_EXIT_CODE
    def end(cmd_ctx: CommandContext, nodes: List[Node]):
        if len(nodes) != 0:
            return SemanticError("'@end' does not accept any arguments")
        error = cmd_ctx.script.popBlock()
        if error:
            return error
        return ExitCode(0)
    def call(cmd_ctx: CommandContext, nodes: List[Node]):
        args = []
        error = nodesToArgs(cmd_ctx, nodes, args)
        if error:
            return error
        if len(args) == 0:
            sys.exit("Error: the @call builtin requires at least one argument")
        program_file = args[0]
        if len(args) > 1:
            sys.exit("Error: @call with more than just a program not implemented")
        if cmd_ctx.script.verification_mode:
            return UNKNOWN_EXIT_CODE
        return runFile(cmd_ctx.script.doverify,
                       program_file,
                       cmd_ctx.script.script_specific_builtin_objects["callerworkdir"].value,
                       cmd_ctx.capture.stdout,
                       cmd_ctx.capture.stderr)

    def haveprog(cmd_ctx: CommandContext, nodes: List[Node]):
        error = disableIncompatibleCapture(cmd_ctx, "@haveprog")
        if error:
            return error
        args = []
        error = nodesToArgs(cmd_ctx, nodes, args)
        if error:
            return error
        if len(args) != 1:
            return SemanticError("'@haveprog' expects 1 arg but got {}".format(len(args)))
        return BOOL_TRUE if which(args[0]) else BOOL_FALSE

def opInvalidTypeError(op, operand):
    return SemanticError("'{}' does not accept objects of type {}".format(op, operand.userTypeDescriptor()))

def operandToBool(op, operand) -> Union[Error,Bool,UnknownBool]:
    if isinstance(operand, Error):
        return operand
    if type(operand) == Bool:
        return operand
    if type(operand) == UnknownBool:
        return UNKNOWN_BOOL
    return opInvalidTypeError(op, operand)

def expandSetArgsCommon(cmd_ctx: CommandContext, arg1: Node, arg2: Node, builtin_name: str) -> Tuple[Union[Error,str],Union[None,StitchObject]]:
    name = expandNode(cmd_ctx, arg1)
    if isinstance(name, Error):
        return name, None
    if type(name) != String:
        return SemanticError("{} requires a String for its 1st argument but got {}".format(builtin_name, name.userTypeDescriptor())), None
    name = name.value
    value = expandNode(cmd_ctx, arg2)
    if isinstance(value, Error):
        return value, None
    return name, value

def expandSetArgs(cmd_ctx: CommandContext, arg1: Node, arg2: Node, builtin_name: str, var_map: Dict[str,StitchObject]) -> Union[Error,bool]:
    name, value = expandSetArgsCommon(cmd_ctx, arg1, arg2, builtin_name)
    if isinstance(name, Error):
        return name

    if ((type(value) == String) or
        (type(value) == Bool)):
        is_unknown = False
    elif ((type(value) == UnknownBool) or
          (type(value) == UnknownString)):
        is_unknown = True
    else:
        return SemanticError("{} requires a String or Bool for its 2nd argument but got {}".format(
            builtin_name, value.userTypeDescriptor())), None

    var_map[name] = value
    return is_unknown

class CompareOp:
    def gt(left, right):
        return left > right
    def lt(left, right):
        return left < right

# builtin objects that do not change and are the same for all scripts
builtin_objects = {
    "note": Builtin("note"),
    "echo": Builtin("echo"),
    "set": Builtin("set"),
    "setarray": Builtin("setarray"),
    "settmp": Builtin("settmp"),
    "multiline": Builtin("multiline"),
    "exitcode": Builtin("exitcode"),
    "call": Builtin("call"),
    "assert": Builtin("assert_"),
    "if": Builtin("if_"),
    "end": Builtin("end"),
    "haveprog": Builtin("haveprog"),
    "setenv": Builtin("setenv"),
    "unsetenv": Builtin("unsetenv"),
    "env": Builtin("env"),
    "false": BOOL_FALSE,
    "true": BOOL_TRUE,
    "not": Builtin("not_"),
    "isfile": Builtin("isfile"),
    "or": OrOperator(),
    "and": AndOperator(),
    "eq": EqOperator(),
    "gt": CompareOperator("gt", CompareOp.gt),
    "lt": CompareOperator("lt", CompareOp.lt),
    # NOTE: this is just temporary for testing
    "emptyarray": Array([]),
}

def which(name):
    extensions = [""] if (os.name != "nt") else os.environ["PATHEXT"].split(";")
    for path in os.environ["PATH"].split(os.pathsep):
        for ext in extensions:
            filename = os.path.join(path, name + ext)
            if os.path.isfile(filename) and os.access(filename, os.X_OK):
                return filename
    return None

class ExpandNodesResult:
    pass
class ExpandNodes:
    class Builtin(ExpandNodesResult):
        def __init__(self, builtin: Builtin):
            self.builtin = builtin
    class BinaryExp(ExpandNodesResult):
        def __init__(self, first: StitchObject, op: BinaryOperator):
            self.first = first
            self.op = op
    class Bool(ExpandNodesResult):
        def __init__(self, value):
            self.value = value
    class ExternalProgram(ExpandNodesResult):
        def __init__(self, args: List[str]):
            self.args = args

def expandNodes(cmd_ctx: CommandContext, nodes: List[Node]) -> Union[Error,ExpandNodesResult]:
    assert(len(nodes) > 0)

    obj = expandNode(cmd_ctx, nodes[0])
    if isinstance(obj, Error):
        return obj
    #if isinstance(obj, BinaryOperator):
    #    return SemanticError("missing operand before '{}'".format(obj))
    if isinstance(obj, Builtin):
        return ExpandNodes.Builtin(obj)

    first_obj = obj
    if len(nodes) == 1:
        obj_list = [obj]
    else:
        obj = expandNode(cmd_ctx, nodes[1])
        if isinstance(obj, Error):
            return obj
        if isinstance(obj, BinaryOperator):
            return ExpandNodes.BinaryExp(first_obj, obj)
        obj_list = [first_obj, obj]

    # TODO: are there other special types to handle here?
    if type(first_obj) == Bool:
        if len(nodes) != 1:
            return SemanticError("unexpected Bool at the start of a command")
        return ExpandNodes.Bool(first_obj)

    # we must be running an external program
    args = []
    for obj in obj_list:
        error = objectToArgs(obj, args)
        if error:
            return error

    error = nodesToArgs(cmd_ctx, nodes, args, start=len(obj_list))
    if error:
        return error

    return ExpandNodes.ExternalProgram(args)

def expandNodesToBool(cmd_ctx: CommandContext, nodes: List[Node], builtin_name: str, allow_cmd_result: bool) -> Union[Error,Bool,UnknownBool]:
    if len(nodes) == 0:
        return SemanticError("{} requires at least 1 argument".format(builtin_name))

    result = expandNodes(cmd_ctx, nodes)
    if isinstance(result, Error):
        return result
    if type(result) == ExpandNodes.Bool:
        return result.value
    if type(result) == ExpandNodes.BinaryExp:
        return runBinaryExpression(cmd_ctx, nodes, result.first, result.op)

    if type(result) == ExpandNodes.Builtin:
        result = getattr(BuiltinMethods, result.builtin.name)(cmd_ctx, nodes[1:])
        if type(result) == Bool or type(result) == UnknownBool:
            return result
        # TODO: there are probably more types to handle here
    else:
        assert(type(result) == ExpandNodes.ExternalProgram)
        result = runExternalProgram(cmd_ctx.script.verification_mode, cmd_ctx.capture.stdout, cmd_ctx.capture.stderr, result.args)
    if isinstance(result, Error):
        return result
    is_unknown_exit_code = type(result) == UnknownExitCode
    assert(type(result) == ExitCode or is_unknown_exit_code)
    if not allow_cmd_result:
        return SemanticError("{} expects a Bool but got a CommandResult".format(builtin_name))
    if is_unknown_exit_code:
        return UNKNOWN_BOOL
    return BOOL_TRUE if (result.value == 0) else BOOL_FALSE

#
# is no longer used, but I think it might be later so keeping for now
#
#def expandOneNodeToBool(cmd_ctx: CommandContext, nodes: List[Node], builtin_name: str) -> Union[Error,Bool,UnknownBool]:
#    if len(nodes) != 1:
#        return SemanticError("{} accepts 1 argument but got {}".format(builtin_name, len(nodes)))
#
#    obj = expandNode(cmd_ctx, nodes[0])
#    if isinstance(obj, Error):
#        return obj
#    if type(obj) == Bool:
#        return obj
#    if type(obj) == CommandResult:
#        if result.stdout:
#            raise Exception("TODO")
#        if result.stderr:
#            raise Exception("TODO")
#        return BOOL_TRUE if (result.exitcode == 0) else BOOL_FALSE
#
#    if type(obj) == UnknownBool or type(obj) == UnknownCommandResult:
#        assert(cmd_ctx.script.verification_mode)
#        return UNKNOWN_BOOL
#
#    return SemanticError("'{}' expects Bool but got {}".format(builtin_name, obj.userTypeDescriptor()))

def expandOneNodeToString(cmd_ctx: CommandContext, nodes: List[Node], builtin_name: str) -> Union[Error,String,UnknownString]:
    if len(nodes) != 1:
        return SemanticError("{} accepts 1 argument but got {}".format(builtin_name, len(nodes)))

    obj = expandNode(cmd_ctx, nodes[0])
    if isinstance(obj, Error):
        return obj
    if type(obj) == String:
        return obj
    if type(obj) == UnknownString:
        assert(cmd_ctx.script.verification_mode)
        return UNKNOWN_STRING
    return SemanticError("'{}' expects String but got {}".format(builtin_name, obj.userTypeDescriptor()))

def runCommandNodes(cmd_ctx: CommandContext, nodes: List[Node]) -> Union[Error,Bool,ExitCode,UnknownBool,UnknownExitCode]:
    assert(len(nodes) > 0)
    # TODO: maybe enable printing this in verification_mode to assist
    #       in triaging SemanticErrors
    if not cmd_ctx.script.verification_mode and cmd_ctx.builtin_prefix_count == 0:
        # todo: is ("+" * (depth+1)) too inneficient?
        msg = "{} {}".format("+" * (cmd_ctx.depth+1), " ".join([n.src for n in nodes]))
        # NOTE: ignore capture_stdout, just always print to console for now
        print(msg)

    # handle @end if we are disabled
    if not cmd_ctx.script.blockStack[-1].enabled:
        first = nodes[0]
        if (type(first) == NodeVariable) and (first.id == "end"):
            if len(nodes) > 1:
                return SemanticError("'@end' does not accept any arguments")
            error = cmd_ctx.script.popBlock()
            if error:
                return error
        return ExitCode(0)

    result = expandNodes(cmd_ctx, nodes)
    if isinstance(result, Error):
        return result
    if type(result) == ExpandNodes.Builtin:
        return getattr(BuiltinMethods, result.builtin.name)(cmd_ctx, nodes[1:])
    if type(result) == ExpandNodes.BinaryExp:
        return runBinaryExpression(cmd_ctx, nodes, result.first, result.op)
    if type(result) == ExpandNodes.Bool:
        return SemanticError("unhandled Bool")
    assert(type(result) == ExpandNodes.ExternalProgram)
    return runExternalProgram(cmd_ctx.script.verification_mode, cmd_ctx.capture.stdout, cmd_ctx.capture.stderr, result.args)

def nodesToArgs(cmd_ctx: CommandContext, nodes: List[Node], args: List[str], start: int = 0) -> Error:
    for i, node in enumerate(nodes[start:], start=start):
        obj = expandNode(cmd_ctx, node)
        if isinstance(obj, Error):
            return obj
        error = objectToArgs(obj, args)
        if error:
            return error
    return None

def nodeToNonArrayArg(cmd_ctx: CommandContext, node: Node):
    obj = expandNode(cmd_ctx, node)
    if isinstance(obj, Error):
        return obj
    if type(obj) == Array:
        return SemanticError("unexpected Array")
    args = []
    error = objectToArgs(obj, args)
    if error:
        return error
    # should be true because we verified this isn't an Array
    assert(len(args) == 1)
    return args[0]

def objectToArgs(obj: StitchObject, args: List[str]) -> Error:
    assert(not isinstance(obj, Error))
    if type(obj) == String:
        args.append(obj.value)
    elif type(obj) == Array:
        args.extend(obj.elements)
    elif type(obj) == UnknownString:
        args.append("<UNKNOWN_STRING>")
    elif type(obj) == Bool or type(obj) == UnknownBool:
        return SemanticError("cannot coerce Bool to String")
    elif isinstance(obj, BinaryOperator):
        return SemanticError("unexpected '{}'".format(obj))
    elif isinstance(obj, Builtin):
        return SemanticError("builtin program '{}' cannot be coerced to a String".format(obj))
    else:
        return SemanticError("TODO: implement objectToArgs for type {} ({})".format(type(obj), obj))

def runExternalProgram(verification_mode: bool, stdout_handler: DataHandler,
                       stderr_handler: DataHandler, args: List[str]) -> Union[Error,ExitCode,UnknownExitCode]:
    if len(args) == 0:
        # NOTE: this can happen if the user executed an expanded empty array
        #       what should we do in this case?
        # NOTE: this is actually a RuntimeError because we currently don't detect
        #       this until the array is expanded at runtime
        return SemanticError("got a command with no arguments, what should the language do here?")

    if verification_mode:
        return UNKNOWN_EXIT_CODE

    prog = args[0]

    if not "/" in prog:
        prog_filename = which(prog)
        if not prog_filename:
            return MissingProgramError(prog)
        # TODO: on linux we can specify a program file, and not modify args
        args[0] = prog_filename

    stdout = None if (type(stdout_handler) == ConsolePrinter) else subprocess.PIPE
    stderr = None if (type(stderr_handler) == ConsolePrinter) else subprocess.PIPE
    result = subprocess.run(args, stdout=stdout, stderr=stderr)
    if type(stdout_handler) != ConsolePrinter:
        stdout_handler.handle(result.stdout.decode("utf8"))
    if type(stderr_handler) != ConsolePrinter:
        stderr_handler.handle(result.stderr.decode("utf8"))
    return ExitCode(result.returncode)

def tryLookupCommandVar(cmd_ctx: CommandContext, name: str):
    ctx = cmd_ctx
    while ctx:
        obj = ctx.var_map.get(name)
        if obj:
            return obj
        ctx = ctx.parent

def tryLookupUserVar(cmd_ctx: CommandContext, name: str):
    obj = tryLookupCommandVar(cmd_ctx, name)
    if obj:
        return obj
    obj = cmd_ctx.script.var_map.get(name)
    if obj:
        return obj
    return None

def tryLookupBuiltinVar(script_ctx: ScriptContext, name: str):
    obj = script_ctx.script_specific_builtin_objects.get(name)
    if obj:
        return obj
    obj = builtin_objects.get(name)
    if obj:
        return obj
    return None

def stdoutOnlyHandler(stdout, multiline):
    if multiline:
        return String(stdout)
    lines = stdout.splitlines()
    if len(lines) == 0:
        return String("")
    elif len(lines) == 1:
        return String(stripNewline(lines[0]))
    return UnexpectedMultilineError(stdout)
    #sys.exit("Error: program '{}' returned {} lines, but command-subtitution requires only 1 line of output.  Prefix the command with '@multiline' to support multiple.".format(str(node.nodes[0]), len(lines)))

def isCaptured(s):
    return s != None

def combineRunResultWithOutputs(cmd_ctx, result):
    if isinstance(result, Error):
        return result

    stdout = None
    stderr = None
    if cmd_ctx.parent:
        if cmd_ctx.capture.stdout != cmd_ctx.parent.capture.stdout:
            assert(type(cmd_ctx.capture.stdout) == StringBuilder)
            stdout = cmd_ctx.capture.stdout.output
        if cmd_ctx.capture.stderr != cmd_ctx.parent.capture.stderr:
            assert(type(cmd_ctx.capture.stderr) == StringBuilder)
            stderr = cmd_ctx.capture.stderr.output

    exitcode = None
    if type(result) == ExitCode:
        is_unknown = False
        exitcode = result.value
    elif type(result) == UnknownExitCode:
        is_unknown = True
        exitcode = 0

    if exitcode != None:
        capture_ec = cmd_ctx.capture.exitcode
        if (not capture_ec) and (exitcode != 0):
            return NonZeroExitCodeError(exitcode, stdout, stderr)

        if capture_ec and (not isCaptured(stdout)) and (not isCaptured(stderr)):
            if is_unknown:
                return UNKNOWN_BOOL
            return BOOL_TRUE if (result.value == 0) else BOOL_FALSE
        if (not capture_ec) and isCaptured(stdout) and (not isCaptured(stderr)):
            if is_unknown:
                return UNKNOWN_STRING
            return stdoutOnlyHandler(stdout, result.multiline)
        if (not capture_ec) and (not isCaptured(stdout)) and isCaptured(stderr):
            if is_unknown:
                return UNKNOWN_STRING
            return String(stderr)

        if is_unknown:
            return UNKNOWN_COMMAND_RESULT
        return CommandResult(result.value, stdout, stderr)

    if type(result) == Bool:
        if cmd_ctx.capture.exitcode or isCaptured(stdout) or isCaptured(stderr):
            raise Exception("ec={} stdout={} stderr={}".format(cmd_ctx.capture.exitcode, isCaptured(stdout), isCaptured(stderr)))
        return result

    raise Exception("expected an ExitCode or Bool but got {}".format(result.userTypeDescriptor()))


# returns an array of strings and builtin objects
def expandNode(cmd_ctx: CommandContext, node: Node) -> StitchObject:
    if type(node) is NodeToken:
        return String(node.s)

    if type(node) is NodeVariable:
        if node.is_at:
            obj = tryLookupBuiltinVar(cmd_ctx.script, node.id)
        else:
            obj = tryLookupUserVar(cmd_ctx, node.id)
        if not obj:
            return SemanticError("'{}' is undefined".format(node.src, node.id))
        return obj

    if type(node) is NodeInlineCommand:
        inline_cmd_ctx = cmd_ctx.createChild()
        result = runCommandNodes(inline_cmd_ctx, node.nodes)
        return combineRunResultWithOutputs(inline_cmd_ctx, result)

    if type(node) is NodeMultiple:
        args = []
        error = nodesToArgs(cmd_ctx, node.nodes, args)
        if error:
            return error
        return String("".join(args))

    raise Exception("codebug, unhandled node type {}".format(type(node)))

def disableIncompatibleCapture(cmd_ctx: CommandContext, error_context: str):
    if cmd_ctx.capture.exitcode:
        return SemanticError("@exitcode is not compatible with {}".format(error_context))
    if cmd_ctx.parent != None and cmd_ctx.capture.stderr != cmd_ctx.parent.capture.stderr:
        return SemanticError("@stderr is not compatible with {}".format(error_context))
    if cmd_ctx.parent != None and cmd_ctx.capture.stdout != cmd_ctx.parent.capture.stdout:
        cmd_ctx.capture.stdout = cmd_ctx.parent.capture.stdout
    return None

def runBinaryExpression(cmd_ctx: CommandContext, nodes: List[Node], first_obj: StitchObject, op: BinaryOperator):
    if cmd_ctx.ambiguous_op:
        return SemanticError("got binary expression inside ambiguous operator '{}', wrap inside (..parenthesis..)".format(cmd_ctx.ambiguous_op))

    error = disableIncompatibleCapture(cmd_ctx, "binary expressions")
    if error:
        return error

    stdout_handler = None
    expression_result = op.initialValue(first_obj)
    if isinstance(expression_result, Error):
        return expression_result
    if isinstance(op, ChainableBinaryOperator) and not cmd_ctx.script.verification_mode and op.shortcircuit(expression_result):
        return expression_result

    index = 1
    while True:
        if index + 1 == len(nodes):
            return SemanticError("missing operand after '{}'".format(op))

        operand_result = expandNode(cmd_ctx, nodes[index+1])
        if type(operand_result) == SemanticError:
            return operand_result
        expression_result = op.apply(cmd_ctx.script.verification_mode, expression_result, operand_result)
        if isinstance(expression_result, Error):
            return expression_result
        if isinstance(op, ChainableBinaryOperator) and not cmd_ctx.script.verification_mode and op.shortcircuit(expression_result):
            return expression_result

        index += 2
        if index == len(nodes):
            return expression_result

        next_op = nodes[index]
        if type(next_op) != NodeVariable:
            if type(next_op) == NodeToken:
                return SemanticError("expected '{}' operator but got token '{}'; commands must be wrapped with (...)".format(op, next_op.s))
            return SemanticError("TODO: good error message for node that was expected to be an operand: {}".format(next_op))
        if next_op.id != op.name:
            return SemanticError("'{}' and '@{}' cannot be chained".format(op, next_op.id))


def runLine(script_ctx: ScriptContext, line: str, stdout_handler: DataHandler, stderr_handler: DataHandler) -> Union[Error,Bool,ExitCode,UnknownBool,UnknownExitCode]:
    nodes = parseTopLevelCommand(line)
    if len(nodes) == 0:
        return ExitCode(0)

    # Note: it seems like it might be better to just print the
    #       line in its source form rather than the expanded form
    #       definitely should have an option for this
    #if script_ctx.print_trace:
    #    msg = "+ {}".format(line)
    #    if capture_stdout:
    #        output += msg + "\n"
    #    else:
    #        print(msg)
    cmd_ctx = CommandContext(
        script_ctx,
        parent=None,
        depth=0,
        capture=Capture(False, stdout_handler, stderr_handler),
        builtin_prefix_count=0,
        ambiguous_op=None
    )
    return runCommandNodes(cmd_ctx, nodes)

def runFileHelper(script_ctx: ScriptContext, filename: str, stdout_handler: DataHandler, stderr_handler: DataHandler) -> ExitCode:
    with open(filename, "r") as file:
        while True:
            line = file.readline()
            if not line:
                break
            line = line.rstrip()
            result = runLine(script_ctx, line, stdout_handler, stderr_handler)
            if isinstance(result, Error):
                return result
            if type(result) == Bool:
                return SemanticError("unhandled Bool")
            if type(result) == ExitCode:
                if result.value != 0:
                    return result
            else:
                assert(script_ctx.verification_mode and (
                    type(result) == UnknownBool or
                    type(result) == UnknownExitCode
                ))

    if len(script_ctx.blockStack) != 1:
        return SemanticError("need more '@end'")
    return ExitCode(0)

def normalizeFilename(filename):
    # TODO: implement this
    return filename

def runFile(doverify: bool, full_filename: str, callerworkdir: str, stdout_handler: DataHandler, stderr_handler: DataHandler) -> ExitCode:

    output = ""

    #full_filename = if os.path.isabsolute(filename) else os.path.os.path.abspath(filename)
    script_ctx = ScriptContext(doverify, full_filename, callerworkdir, verification_mode=False)
    if script_ctx.doverify:
        normalized_filename = normalizeFilename(full_filename)
        if not normalized_filename in script_ctx.verify_started:
            script_ctx.verify_started[normalized_filename] = True
            script_ctx.verification_mode = True
            print("stitch: DEBUG: verifying '{}'".format(full_filename))
            stdout_builder = StringBuilder()
            stderr_builder = StringBuilder()
            result = runFileHelper(script_ctx, full_filename, stdout_builder, stderr_builder)
            script_ctx.verification_mode = False
            if isinstance(result, Error):
                # This can happen if the arguments of an assert are known at "verification time"
                # i.e. @assert @true
                if type(result) == AssertError:
                    print("{}: AssertError: {}".format(full_filename, result.src))
                else:
                    assert(type(result) == SemanticError)
                    print("{}: SemanticError: {}".format(full_filename, result.msg))
                return result
            # I'm allowing things to print to stdout during verification mode
            # because this can allow other mechnisms to get "further" in verification
            if len(stdout_builder.output) > 0:
                pass
                #sys.exit("something printed to stdout during verification mode???")
            if len(stderr_builder.output) > 0:
                pass
                #sys.exit("something printed to stderr during verification mode???")
            print("stitch: DEBUG: verification done on '{}'".format(full_filename))

    return runFileHelper(script_ctx, full_filename, stdout_handler, stderr_handler)

def resolveCallerWorkdir(sandbox_path: str) -> str:
    # TODO: is this the same var on all posix operating systems?
    env_dir_varname = "PWD"

    cwd = os.getcwd()
    env_dir = os.environ.get(env_dir_varname)

    if cwd == sandbox_path:
        if not env_dir:
            # NOTE: we make a concession here that makes it illegal to run stitch scripts from inside the sandbox
            #       path directly, but I think the benefit outweighs it.
            sys.exit("the current directory is the sandbox path '{}' and {} is not set, unable to recover @callerworkdir".format(sandbox_path, env_dir_varname))

        if env_dir == sandbox_path:
            sys.exit("the current directory and {} are the sandbox path '{}', unable to recover @callerworkdir".format(env_dir_varname, sandbox_path))

        return env_dir

    if not env_dir:
        if os.name == "nt":
            os.environ[env_dir_varname] = cwd
            return cwd
        sys.exit("TODO: {} is not set, should we just set it?  Or maybe this OS puts {} into a different variable?".format(env_dir_varname, env_dir_varname))

    if cwd != env_dir:
        sys.exit("the current directory '{}' and {} '{}' don't agree".format(cwd, env_dir_varname, env_dir))

    return cwd

# try to get rid of CWD state by going to a temporary readonly directory
def sandboxCallerWorkdir() -> str:
    if os.name == "nt":
        sandbox_path = os.path.join(os.getenv("TEMP"), "stitch-sandbox")
    else:
        sandbox_path = "/tmp/stitch-sandbox"

    callerworkdir = resolveCallerWorkdir(sandbox_path)

    if not os.path.exists(sandbox_path):
        os.mkdir(sandbox_path)
    os.chdir(sandbox_path)
    return callerworkdir


def main():
    cmd_args = sys.argv[1:]
    if len(cmd_args) == 0:
        sys.exit("Usage: stitch FILE")
    if len(cmd_args) != 1:
        sys.exit("Error: too many command-line arguments")
    filename = cmd_args[0]
    full_filename = os.path.abspath(filename)

    callerworkdir = sandboxCallerWorkdir()

    doverify = True
    result = runFile(doverify, full_filename, callerworkdir, CONSOLE_PRINTER, CONSOLE_PRINTER)
    if isinstance(result, Error):
        prefix = "Semantic" if (type(result) == SemanticError) else ""
        print("{}: {}Error: {}".format(filename, prefix, result.msg))
        sys.exit(1)

    assert(type(result) == ExitCode)
    if result.value != 0:
        sys.exit("Error: the last progam exited with code {}".format(result.value))

if __name__ == "__main__":
    main()
