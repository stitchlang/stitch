#!/usr/bin/env python3
import sys
import os
import subprocess
import re
from abc import ABC, abstractmethod
from enum import Enum
from typing import List, Dict, Set, Union, Tuple, Optional

import lex
import parse

def eprint(msg_str: str):
    print(msg_str, file=sys.stderr)

def stripNewline(s: bytes):
    assert(isinstance(s, bytes))
    if len(s) > 0 and s[-1] == b"\n":
        if len(s) > 2 and s[-2] == b"\r":
            return s[:-2]
        return s[:-1]
    return s

class StitchObject:
    @staticmethod
    def userTypeDescriptor():
        raise Exception("this Object has not implemented the userTypeDescriptor method")

class Bool(StitchObject):
    def __init__(self, value):
        self.value = value
    @staticmethod
    def userTypeDescriptor():
        return "Bool"
BOOL_FALSE = Bool(False)
BOOL_TRUE = Bool(True)

class BuiltinExpandType(Enum):
    ParseNodes = 0
    ExpandNodesResult = 1
    Objects = 2
    Strings = 3
class BuiltinReturnType(Enum):
    ExitCode = 0
    Bool = 1
    Array = 2

class Builtin(StitchObject):
    def __init__(self, python_name_str: str, expand_type: BuiltinExpandType, return_type: BuiltinReturnType, arg_count: Optional[int] = None):
        name_str = python_name_str.rstrip("_")
        self.name = name_str.encode('utf8')
        self.python_name_str = python_name_str
        self.atname_str = "@" + name_str
        self.expand_type = expand_type
        self.arg_count = arg_count
        self.return_type = return_type
    def __repr__(self):
        return self.atname_str
    @staticmethod
    def userTypeDescriptor():
        return "Builtin"
class String(StitchObject):
    def __init__(self, value: bytes):
        assert(type(value) == bytes)
        self.value = value
    @staticmethod
    def userTypeDescriptor():
        return "String"
class Array(StitchObject):
    def __init__(self, elements: List[bytes]):
        self.elements = elements
    @staticmethod
    def userTypeDescriptor():
        return "Array"

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

class UnknownArray(Unknown):
    def __init__(self):
        Unknown.__init__(self, Array)
    @staticmethod
    def userTypeDescriptor():
        return "Array"
UNKNOWN_ARRAY = UnknownArray()

class BinaryOperator(StitchObject):
    def __init__(self, name: bytes):
        assert(isinstance(name, bytes))
        self.name = name
        self.src_name_str = "@" + name.decode('ascii')
    def __repr__(self):
        return self.src_name_str
    @abstractmethod
    def initialValue(self, operand: StitchObject):
        pass
    @abstractmethod
    def apply(self, verification_mode: bool, left: Bool, right: StitchObject):
        pass
class ChainableBinaryOperator(BinaryOperator):
    def __init__(self, name: bytes):
        BinaryOperator.__init__(self, name)
    @abstractmethod
    def shortcircuit(self, result: Bool):
        return not result.value
class AndOperator(ChainableBinaryOperator):
    def __init__(self):
        ChainableBinaryOperator.__init__(self, b"and")
    def initialValue(self, operand: StitchObject):
        return operandToBool(self, operand)
    def apply(self, verification_mode: bool, left: Bool, right: StitchObject):
        assert(verification_mode or left.value)
        right_result = operandToBool(self, right)
        if isinstance(right_result, Error) or isinstance(right_result, UnknownBool):
            return right_result
        return right_result
    def shortcircuit(self, result: Bool):
        return not result.value
class OrOperator(ChainableBinaryOperator):
    def __init__(self):
        ChainableBinaryOperator.__init__(self, b"or")
    def initialValue(self, operand):
        return operandToBool(self, operand)
    def apply(self, verification_mode: bool, left: Bool, right: StitchObject):
        assert(verification_mode or not left.value)
        right_result = operandToBool(self, right)
        if isinstance(right_result, Error) or isinstance(right_result, UnknownBool):
            return right_result
        return right_result
    def shortcircuit(self, result: Bool):
        return result.value
class EqOperator(BinaryOperator):
    def __init__(self):
        BinaryOperator.__init__(self, b"eq")
    def initialValue(self, operand):
        if isinstance(operand, String):
            return operand
        if isinstance(operand, UnknownString):
            return UNKNOWN_BOOL
        return opInvalidTypeError(self, operand)
    def apply(self, verification_mode: bool, left, right: StitchObject):
        if isinstance(left, String):
            if isinstance(right, String):
                return BOOL_TRUE if (left.value == right.value) else BOOL_FALSE
            if isinstance(right, UnknownString):
                return UNKNOWN_BOOL
            return opInvalidTypeError(self, right)
        assert(isinstance(left, UnknownString))
        return UNKNOWN_BOOL
class CompareOperator(BinaryOperator):
    def __init__(self, name: bytes, func):
        BinaryOperator.__init__(self, name)
        self.func = func
    def initialValue(self, operand):
        if isinstance(operand, String):
            # TODO: return semantic error if not a valid integer
            return int(operand.value)
        if isinstance(operand, UnknownString):
            return None
        return opInvalidTypeError(self, operand)
    def apply(self, verification_mode: bool, left, right: StitchObject):
        if left is None:
            if (not isinstance(right, String)) and (not isinstance(right, UnknownString)):
                return opInvalidTypeError(self, right)
            return UNKNOWN_BOOL
        assert(isinstance(left, int))
        if isinstance(right, String):
            # TODO: return semantic error if not a valid integer
            right_int = int(right.value)
            return BOOL_TRUE if self.func(left, right_int) else BOOL_FALSE
        if isinstance(right, UnknownString):
            return UNKNOWN_BOOL
        return opInvalidTypeError(self, right)

class CommandResult(StitchObject):
    def __init__(self, exitcode: int, stdout: Optional[str], stderr: Optional[str]):
        self.exitcode = exitcode
        self.stdout = stdout
        self.stderr = stderr
    @staticmethod
    def userTypeDescriptor():
        return "CommandResult"
    def __repr__(self):
        return "CommandResult(exit={},stderr='{}',stdout='{}')".format(
            self.exitcode, self.stderr, self.stdout)

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
    def __init__(self, message):
        self.message = message
    def __repr__(self):
        return self.message
class SyntaxError(Error):
    def __init__(self, filename_str: str, src: bytes, lex_error: lex.SyntaxError):
        assert(isinstance(filename_str, str))
        line, column = lex.countLinesAndColumns(src[:lex_error.pos])
        super().__init__("{}(line {} column {}) {}".format(filename_str, line, column, str(lex_error)))
class SemanticError(Error):
    def __init__(self, message):
        super().__init__(message)
class CannotCoerceToStringError(SemanticError):
    def __init__(self, uncoercable_type):
        super().__init__("cannot coerce {} to String".format(uncoercable_type))
        self.uncoercable_type = uncoercable_type
class AssertError(Error):
    def __init__(self, nodes: List[parse.Node]):
        Error.__init__(self, "@assert failed")
        self.nodes = nodes
class NonZeroExitCodeError(Error):
    def __init__(self, exitcode: int, stdout: Optional[str], stderr: Optional[str]):
        assert(exitcode != 0)
        Error.__init__(self, "command failed with exit code {}".format(exitcode))
        self.exitcode = exitcode
        self.stdout = stdout
        self.stderr = stderr
class MissingProgramError(Error):
    def __init__(self, filename):
        Error.__init__(self, "unable to find program '{}' in PATH".format(filename))
class MissingStitchScript(Error):
    def __init__(self, prog_str: str):
        Error.__init__(self, "stitch script '{}' does not exist".format(prog_str))
class UnexpectedMultilineError(Error):
    def __init__(self, stdout):
        Error.__init__(self, "missing '@multiline', got this multiline output\n----\n{}----\n".format(stdout.decode('utf8')))
class UndefinedEnvironmentVariableError(Error):
    def __init__(self, name_str: str):
        Error.__init__(self, "undefined environment variable '{}'".format(name_str))
        self.name_str = name_str

# These ExitCode and UnknownExitCode types are currently only used internally
class ExitCode:
    def __init__(self, value: int):
        self.value = value
        self.multiline = False
class UnknownExitCode:
    pass
UNKNOWN_EXIT_CODE = UnknownExitCode()

class GlobalContext:
    def __init__(self, doverify: bool, callerworkdir: bytes):
        assert(type(callerworkdir) == bytes)
        self.doverify = doverify
        self.callerworkdir = callerworkdir
        self.verify_started: Set[str] = set()

class ScriptContext:
    class Block:
        def __init__(self, enabled: Optional[bool]):
            self.enabled = enabled

    # TODO: add fields for logging/tracing options?
    def __init__(self, global_ctx: GlobalContext, scriptfile: Optional[bytes], src: bytes, verification_mode: bool):
        assert((scriptfile is None) or (type(scriptfile) == bytes))
        self.global_ctx = global_ctx
        self.src = src
        self.script_specific_builtin_objects = {
            b"scriptfile": String(scriptfile if scriptfile else b"<the -c command>"),
            b"scriptdir": String(os.path.dirname(scriptfile) if scriptfile else b"<the -c command>"),
            # NOTE: callerworkdir will need to be forwarded to any sub-scripts
            #       maybe I can just use an environment variable
            b"callerworkdir": String(global_ctx.callerworkdir),
        }
        self.var_map: Dict[bytes,StitchObject] = {}
        self.verification_mode = verification_mode
        self.blockStack: List[ScriptContext.Block] = [ScriptContext.Block(enabled=True)]
        self.allstringliterals = False
    def pushBlock(self, enabled: bool) -> None:
        self.blockStack.append(ScriptContext.Block(enabled))
    def popBlock(self) -> Optional[Error]:
        self.blockStack.pop()
        if len(self.blockStack) == 0:
            return SemanticError("too many '@end'")
        return None

class DataHandler(ABC):
    @abstractmethod
    def isEmpty(self):
        pass
    @abstractmethod
    def handle(self, s: bytes):
        pass
class StringBuilder(DataHandler):
    def __init__(self):
        self.output = b""
    @staticmethod
    def descriptor():
        return "capture"
    def isEmpty(self):
        return len(self.output) == 0
    def handle(self, s: bytes):
        assert(type(s) == bytes)
        if len(s) > 0:
            self.output += s
            if s[-1] != ord("\n"):
                self.output += b"\n"
class ConsolePrinter(DataHandler):
    @staticmethod
    def descriptor():
        return "console"
    def isEmpty(self):
        return True
    def handle(self, s: bytes):
        assert(type(s) == bytes)
        if len(s) > 0:
            if s[-1] == b'\n':
                sys.stdout.buffer.write(s)
            else:
                sys.stdout.buffer.write(s)
                sys.stdout.buffer.write(b'\n')
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
            is_binary_expr: bool,
            parent: Optional['CommandContext'],
            depth: int,
            capture: Capture,
            builtin_prefix_count: int,
            ambiguous_op: Optional[str],
            var_map: Dict[bytes,StitchObject] = {}
    ):
        self.script = script
        self.is_binary_expr = is_binary_expr
        self.parent = parent
        self.depth = depth
        self.capture = capture
        self.builtin_prefix_count = builtin_prefix_count
        # true if the current command is inside an ambiguous operator (currently just @not)
        self.ambiguous_op = ambiguous_op
        self.var_map = var_map
    def createChild(self, is_binary_expr: bool) -> 'CommandContext':
        return type(self)(self.script, is_binary_expr, self, self.depth+1,
                          Capture(exitcode=False,stdout=StringBuilder(),stderr=self.capture.stderr),
                          builtin_prefix_count=0, ambiguous_op=None)
    def nextBuiltin(self, ambiguous_op: Optional[str]) -> 'CommandContext':
        assert(not self.is_binary_expr)
        return type(self)(self.script, self.is_binary_expr, self.parent, self.depth, self.capture,
                          self.builtin_prefix_count + 1, ambiguous_op, self.var_map)

class BuiltinMethods:
    @staticmethod
    def allstringliterals(cmd_ctx: CommandContext, nodes: List[parse.Node]):
        assert(len(nodes) == 0)
        cmd_ctx.script.allstringliterals = True
        return ExitCode(0)
    @staticmethod
    def note(cmd_ctx: CommandContext, nodes: List[parse.Node]):
        return ExitCode(0)
    @staticmethod
    def echo(cmd_ctx: CommandContext, args: List[bytes]):
        if len(args) > 0:
            cmd_ctx.capture.stdout.handle(b" ".join(args))
        return ExitCode(0)
    # TODO: maybe remove this?  Maybe @pushscope/@popscope?
    @staticmethod
    def settmp(cmd_ctx: CommandContext, nodes: List[parse.Node]):
        if len(nodes) < 3:
            return SemanticError("@settmp requires at least 3 arguments but got {}".format(len(nodes)))
        is_unknown = expandSetArgs(cmd_ctx, nodes[0], nodes[1], "@settmp", cmd_ctx.var_map)
        if isinstance(is_unknown, Error):
            return is_unknown
        #print("DEBUG: settmp '{}' to '{}'".format(varname, value))
        return runCommandNodes(cmd_ctx.nextBuiltin(ambiguous_op=None), nodes[2:])
    @staticmethod
    def setenv(cmd_ctx: CommandContext, args: List[bytes]):
        assert(len(args) == 2)
        name = args[0]
        value = args[1]
        if not cmd_ctx.script.verification_mode:
            os.environ[name.decode('utf8')] = value.decode('utf8')
            return ExitCode(0)
        return UNKNOWN_EXIT_CODE
    @staticmethod
    def unsetenv(cmd_ctx: CommandContext, args: List[bytes]):
        assert(len(args) == 1)
        name = args[0]
        if cmd_ctx.script.verification_mode:
            return UNKNOWN_EXIT_CODE
        name_str = name.decode('utf8')
        if not name_str in os.environ:
            return UndefinedEnvironmentVariableError(name_str)
        del os.environ[name_str]
        return ExitCode(0)
    @staticmethod
    def env(cmd_ctx: CommandContext, args: List[bytes]):
        assert(len(args) == 1)
        if cmd_ctx.script.verification_mode:
            return UNKNOWN_EXIT_CODE
        name = args[0]
        name_str = name.decode('utf8')
        value = os.environ.get(name_str)
        if value is None:
            return UndefinedEnvironmentVariableError(name_str)
        cmd_ctx.capture.stdout.handle(value.encode('utf8'))
        return ExitCode(0)
    # NOTE: envdefault uses raw ParseNodes to support lazy expansion of the default value
    @staticmethod
    def envdefault(cmd_ctx: CommandContext, nodes: List[parse.Node]):
        assert(len(nodes) == 2)
        name = expandNode(cmd_ctx, nodes[0], ExpandNodeErrorContext())
        if isinstance(name, Error):
            return name
        if not isinstance(name, String) and not isinstance(name, UnknownString):
            return SemanticError("@envdefault requires a String for its first argument but got {}".format(name.userTypeDescriptor()))
        if not cmd_ctx.script.verification_mode:
            assert(isinstance(name, String))
            env_value = os.environ.get(name.value.decode('utf8'))
            if env_value:
                cmd_ctx.capture.stdout.handle(env_value.encode('utf8'))
                return ExitCode(0)

        # NOTE: we only expand the default value if the environment variable does not exist
        default = expandNode(cmd_ctx, nodes[1], ExpandNodeErrorContext())
        if isinstance(default, Error):
            return default
        if not isinstance(default, String) and not isinstance(default, UnknownString):
            return SemanticError("@envdefault requires a String for its second argument but got {}".format(default.userTypeDescriptor()))
        if cmd_ctx.script.verification_mode:
            return UNKNOWN_EXIT_CODE
        assert(isinstance(name, String))
        assert(isinstance(default, String))
        cmd_ctx.capture.stdout.handle(default.value)
        return ExitCode(0)
    @staticmethod
    def multiline(cmd_ctx: CommandContext, nodes: List[parse.Node]):
        if len(nodes) == 0:
            return SemanticError("@multiline requires at least 1 argument")
        if cmd_ctx.parent is None:
            return SemanticError("the @multiline builtin is only supported within an (..inline command..)")
        if cmd_ctx.capture.stdout == cmd_ctx.parent.capture.stdout:
            return SemanticError("got @multiline but stdout is not being captured?  what's going on?")
        result = runCommandNodes(cmd_ctx.nextBuiltin(ambiguous_op=None), nodes)
        if isinstance(result, Error) or isinstance(result, UnknownExitCode) or isinstance(result, UnknownBool):
            return result
        if isinstance(result, Bool):
            return SemanticError("@multiline does not accept Bool")
        assert(isinstance(result, ExitCode))
        result.multiline = True
        return result
    @staticmethod
    def exitcode(cmd_ctx: CommandContext, nodes: List[parse.Node]):
        if len(nodes) == 0:
            return SemanticError("@exitcode requires at least 1 argument")
        if cmd_ctx.parent is None:
            return SemanticError("the @exitcode builtin is only supported within an (..inline command..)")
        # this should be impossible because disableCaptureModifiers will catch it
        assert(not cmd_ctx.capture.exitcode)
        cmd_ctx.capture.exitcode = True
        # this should have been set by disableCaptureModifiers
        # because this builtin sets return_bool to True
        assert(cmd_ctx.capture.stdout == cmd_ctx.parent.capture.stdout)
        return runCommandNodes(cmd_ctx.nextBuiltin(ambiguous_op=None), nodes)
    @staticmethod
    def not_(cmd_ctx: CommandContext, nodes: List[parse.Node]):
        result = expandNodesToBool(cmd_ctx.nextBuiltin(ambiguous_op="@not"), nodes, "@not", True)
        if isinstance(result, Error) or isinstance(result, UnknownBool):
            return result
        assert(isinstance(result, Bool))
        return BOOL_FALSE if result.value else BOOL_TRUE
    @staticmethod
    def isfile(cmd_ctx: CommandContext, args: List[bytes]):
        assert(len(args) == 1)
        if cmd_ctx.script.verification_mode:
            return UNKNOWN_BOOL
        return BOOL_TRUE if os.path.isfile(args[0]) else BOOL_FALSE
    @staticmethod
    def isdir(cmd_ctx: CommandContext, args: List[bytes]):
        assert(len(args) == 1)
        if cmd_ctx.script.verification_mode:
            return UNKNOWN_BOOL
        return BOOL_TRUE if os.path.isdir(args[0]) else BOOL_FALSE
    @staticmethod
    def haveprog(cmd_ctx: CommandContext, args: List[bytes]):
        assert(len(args) == 1)
        if cmd_ctx.script.verification_mode:
            return UNKNOWN_BOOL
        return BOOL_TRUE if which(args[0]) else BOOL_FALSE

    @staticmethod
    def assert_(cmd_ctx: CommandContext, nodes: List[parse.Node]):
        result = expandNodesToBool(cmd_ctx.nextBuiltin(ambiguous_op=None), nodes, "@assert", False)
        if isinstance(result, Error):
            return result
        if isinstance(result, UnknownBool):
            return UNKNOWN_EXIT_CODE
        assert(isinstance(result, Bool))
        if not result.value:
            return AssertError(nodes)
        return ExitCode(0)
    @staticmethod
    def if_(cmd_ctx: CommandContext, nodes: List[parse.Node]):
        result = expandNodesToBool(cmd_ctx.nextBuiltin(ambiguous_op=None), nodes, "@if", True)
        if isinstance(result, Error):
            return result
        if isinstance(result, Bool):
            cmd_ctx.script.pushBlock(enabled=result.value)
            return ExitCode(0)
        assert(cmd_ctx.script.verification_mode and (
            isinstance(result, UnknownBool) or
            isinstance(result, UnknownCommandResult)))
        cmd_ctx.script.pushBlock(enabled=True)
        return UNKNOWN_EXIT_CODE
    @staticmethod
    def end(cmd_ctx: CommandContext, nodes: List[parse.Node]):
        assert(len(nodes) == 0)
        error = cmd_ctx.script.popBlock()
        if error:
            return error
        return ExitCode(0)
    @staticmethod
    def call(cmd_ctx: CommandContext, args: List[bytes]):
        if len(args) == 0:
            return SemanticError("@call requires at least one argument")
        program_file = args[0]
        if len(args) > 1:
            sys.exit("Error: @call with more than just a program not implemented")
        if cmd_ctx.script.verification_mode:
            return UNKNOWN_EXIT_CODE
        # TODO: should I be caching files?
        try:
            with open(program_file, "rb") as file:
                # TODO: try mmap if it is supported
                src = file.read()
        except FileNotFoundError:
            return MissingStitchScript(program_file.decode('utf8'))
        result = runFile(cmd_ctx.script.global_ctx,
                         program_file,
                         src,
                         cmd_ctx.capture.stdout,
                         cmd_ctx.capture.stderr)
        if isinstance(result, Error):
            return result
        assert(isinstance(result, ExitCode))
        return result

    # arrays are limited to strings only right now, so we can expand all arguments to strings
    @staticmethod
    def array(cmd_ctx: CommandContext, args: List[bytes]):
        return Array(args)
    @staticmethod
    def lines2array(cmd_ctx: CommandContext, args: List[bytes]):
        assert(len(args) == 1)
        return Array(args[0].splitlines())

def opInvalidTypeError(op: BinaryOperator, operand: StitchObject):
    #raise Exception("'{}' does not accept objects of type {}".format(op, operand.userTypeDescriptor()))
    return SemanticError("'{}' does not accept objects of type {}".format(op, operand.userTypeDescriptor()))

def operandToBool(op: BinaryOperator, operand: StitchObject) -> Union[Error,Bool,UnknownBool]:
    if isinstance(operand, Error):
        return operand
    if isinstance(operand, Bool):
        return operand
    if isinstance(operand, UnknownBool):
        return UNKNOWN_BOOL
    return opInvalidTypeError(op, operand)

# TODO: probably remove this (after settmp is removed?)
def expandSetArgsCommon(cmd_ctx: CommandContext, arg1: parse.Node, arg2: parse.Node, builtin_name_str: str) -> Union[Error,Tuple[bytes,StitchObject]]:
    name = expandNode(cmd_ctx, arg1, ExpandNodeErrorContext())
    if isinstance(name, Error):
        return name
    if not isinstance(name, String):
        return SemanticError("{} requires a String for its 1st argument but got {}".format(builtin_name_str, name.userTypeDescriptor()))
    value = expandNode(cmd_ctx, arg2, ExpandNodeErrorContext())
    if isinstance(value, Error):
        return value
    return (name.value, value)

# TODO: probably remove this (after settmp is removed?)
def expandSetArgs(cmd_ctx: CommandContext, arg1: parse.Node, arg2: parse.Node, builtin_name_str: str, var_map: Dict[bytes,StitchObject]) -> Union[Error,bool]:
    pair = expandSetArgsCommon(cmd_ctx, arg1, arg2, builtin_name_str)
    if isinstance(pair, Error):
        return pair

    name, value = pair
    if isinstance(value, String) or isinstance(value, Bool):
        is_unknown = False
    elif isinstance(value, UnknownBool) or isinstance(value, UnknownString):
        is_unknown = True
    else:
        return SemanticError("{} requires a String or Bool for its 2nd argument but got {}".format(
            builtin_name_str, value.userTypeDescriptor()))

    var_map[name] = value
    return is_unknown

class CompareOp:
    def gt(left, right):
        return left > right
    def lt(left, right):
        return left < right

# builtin objects that do not change and are the same for all scripts
builtin_objects = {
    b"allstringliterals": Builtin("allstringliterals", BuiltinExpandType.ParseNodes, BuiltinReturnType.ExitCode, arg_count=0),
    b"note": Builtin("note", BuiltinExpandType.ParseNodes, BuiltinReturnType.ExitCode),
    b"echo": Builtin("echo", BuiltinExpandType.Strings, BuiltinReturnType.ExitCode),
    b"settmp": Builtin("settmp", BuiltinExpandType.ParseNodes, BuiltinReturnType.ExitCode),
    b"multiline": Builtin("multiline", BuiltinExpandType.ParseNodes, BuiltinReturnType.ExitCode),
    b"exitcode": Builtin("exitcode", BuiltinExpandType.ParseNodes, BuiltinReturnType.Bool),
    b"call": Builtin("call", BuiltinExpandType.Strings, BuiltinReturnType.ExitCode),
    b"assert": Builtin("assert_", BuiltinExpandType.ParseNodes, BuiltinReturnType.ExitCode),
    b"if": Builtin("if_", BuiltinExpandType.ParseNodes, BuiltinReturnType.ExitCode),
    b"end": Builtin("end", BuiltinExpandType.ParseNodes, BuiltinReturnType.ExitCode, arg_count=0),
    b"haveprog": Builtin("haveprog", BuiltinExpandType.Strings, BuiltinReturnType.Bool, arg_count=1),
    b"setenv": Builtin("setenv", BuiltinExpandType.Strings, BuiltinReturnType.ExitCode, arg_count=2),
    b"unsetenv": Builtin("unsetenv", BuiltinExpandType.Strings, BuiltinReturnType.ExitCode, arg_count=1),
    b"env": Builtin("env", BuiltinExpandType.Strings, BuiltinReturnType.ExitCode, arg_count=1),
    # NOTE: envdefault must take ParseNodes to support lazy expansion of the default value
    b"envdefault": Builtin("envdefault", BuiltinExpandType.ParseNodes, BuiltinReturnType.ExitCode, arg_count=2),
    b"false": BOOL_FALSE,
    b"true": BOOL_TRUE,
    b"not": Builtin("not_", BuiltinExpandType.ParseNodes, BuiltinReturnType.Bool),
    b"isfile": Builtin("isfile", BuiltinExpandType.Strings, BuiltinReturnType.Bool, arg_count=1),
    b"isdir": Builtin("isdir", BuiltinExpandType.Strings, BuiltinReturnType.Bool, arg_count=1),
    b"or": OrOperator(),
    b"and": AndOperator(),
    b"eq": EqOperator(),
    b"gt": CompareOperator(b"gt", CompareOp.gt),
    b"lt": CompareOperator(b"lt", CompareOp.lt),
    b"array": Builtin("array", BuiltinExpandType.Strings, BuiltinReturnType.Array),
    b"lines2array": Builtin("lines2array", BuiltinExpandType.Strings, BuiltinReturnType.Array, arg_count=1),
}

def which(name):
    extensions = [b""] if (os.name != "nt") else [e.encode('utf8') for e in os.environ["PATHEXT"].split(";")]
    for path in os.environ["PATH"].split(os.pathsep):
        path_bytes = path.encode('utf8')
        for ext in extensions:
            filename = os.path.join(path_bytes, name + ext)
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
        def __init__(self, args: List[bytes]):
            self.args = args

# Use to provide extra context for better error messages
class ExpandNodeErrorContext:
    def __init__(self, inside_multiple: bool = False):
        self.inside_multiple = inside_multiple

def expandNodes(cmd_ctx: CommandContext, nodes: List[parse.Node]) -> Union[Error,ExpandNodesResult]:
    assert(len(nodes) > 0)

    obj = expandNode(cmd_ctx, nodes[0], ExpandNodeErrorContext())
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
        obj = expandNode(cmd_ctx, nodes[1], ExpandNodeErrorContext())
        if isinstance(obj, Error):
            return obj
        if isinstance(obj, BinaryOperator):
            return ExpandNodes.BinaryExp(first_obj, obj)
        obj_list = [first_obj, obj]

    # TODO: are there other special types to handle here?
    if isinstance(first_obj, Bool) or isinstance(first_obj, UnknownBool):
        if len(nodes) != 1:
            return SemanticError("unexpected Bool at the start of a command")
        return ExpandNodes.Bool(first_obj)

    # we must be running an external program
    args: List[bytes] = []
    for obj in obj_list:
        obj_args_error = objectToArgs(obj, args)
        if obj_args_error:
            return obj_args_error

    error = nodesToArgs(cmd_ctx, nodes, args, ExpandNodeErrorContext(), start=len(obj_list))
    if error:
        return error

    return ExpandNodes.ExternalProgram(args)

def expandNodesToBool(cmd_ctx: CommandContext, nodes: List[parse.Node], builtin_name_str: str, allow_cmd_result: bool) -> Union[Error,Bool,UnknownBool]:
    if len(nodes) == 0:
        return SemanticError("{} requires at least 1 argument".format(builtin_name_str))

    expand_result = expandNodes(cmd_ctx, nodes)
    if isinstance(expand_result, Error):
        return expand_result
    if isinstance(expand_result, ExpandNodes.Bool):
        return expand_result.value
    if isinstance(expand_result, ExpandNodes.BinaryExp):
        return runBinaryExpression(cmd_ctx, nodes, expand_result.first, expand_result.op)

    if isinstance(expand_result, ExpandNodes.Builtin):
        next_result = runBuiltin(cmd_ctx, expand_result.builtin, nodes[1:])
        if isinstance(next_result, Bool) or isinstance(next_result, UnknownBool):
            return next_result
        # TODO: there are probably more types to handle here
    else:
        assert(isinstance(expand_result, ExpandNodes.ExternalProgram))
        next_result = runExternalProgram(cmd_ctx.script.verification_mode, cmd_ctx.capture.stdout, cmd_ctx.capture.stderr, expand_result.args)
    if isinstance(next_result, Error):
        return next_result
    is_unknown_exit_code = isinstance(next_result, UnknownExitCode)
    assert(isinstance(next_result, ExitCode) or is_unknown_exit_code)
    if not allow_cmd_result:
        return SemanticError("{} expects a Bool but got a CommandResult".format(builtin_name_str))
    if is_unknown_exit_code:
        return UNKNOWN_BOOL
    return BOOL_TRUE if (next_result.value == 0) else BOOL_FALSE

#
# is no longer used, but I think it might be later so keeping for now
#
#def expandOneNodeToBool(cmd_ctx: CommandContext, nodes: List[parse.Node], builtin_name_str: str) -> Union[Error,Bool,UnknownBool]:
#    if len(nodes) != 1:
#        return SemanticError("{} accepts 1 argument but got {}".format(builtin_name_str, len(nodes)))
#
#    obj = expandNode(cmd_ctx, nodes[0], ExpandNodeErrorContext())
#    if isinstance(obj, Error):
#        return obj
#    if isinstance(obj, Bool):
#        return obj
#    if isinstance(obj, CommandResult):
#        if result.stdout:
#            raise Exception("TODO")
#        if result.stderr:
#            raise Exception("TODO")
#        return BOOL_TRUE if (result.exitcode == 0) else BOOL_FALSE
#
#    if isinstance(obj, UnknownBool) or isinstance(obj, UnknownCommandResult):
#        assert(cmd_ctx.script.verification_mode)
#        return UNKNOWN_BOOL
#
#    return SemanticError("'{}' expects Bool but got {}".format(builtin_name_str, obj.userTypeDescriptor()))

def enforceBuiltinArgCount(builtin: Builtin, actual: int) -> Optional[Error]:
    if builtin.arg_count and (builtin.arg_count != actual):
        suffix = "" if (builtin.arg_count == 1) else "s"
        return SemanticError("{} takes {} argument{} but got {}".format(
            builtin, builtin.arg_count, suffix, actual))
    return None

def runBuiltin(cmd_ctx: CommandContext, builtin: Builtin, nodes: List[parse.Node]):
    if builtin.return_type != BuiltinReturnType.ExitCode:
        error = disableCaptureModifiers(cmd_ctx, builtin.atname_str)
        if error:
            return error

    func = getattr(BuiltinMethods, builtin.python_name_str)
    if builtin.expand_type == BuiltinExpandType.ParseNodes:
        error = enforceBuiltinArgCount(builtin, len(nodes))
        if error:
            return error
        return func(cmd_ctx, nodes)
    if builtin.expand_type == BuiltinExpandType.Strings:
        args: List[bytes] = []
        error = nodesToArgs(cmd_ctx, nodes, args, ExpandNodeErrorContext())
        if error:
            if isinstance(error, CannotCoerceToStringError):
                if builtin.arg_count is None:
                    requires = "Strings"
                elif builtin.arg_count == 1:
                    requires = "a String"
                else:
                    requires = "{} Strings".format(builtin.arg_count)
                return SemanticError("{} requires {} but got {}".format(builtin, requires, error.uncoercable_type))
            return error
        error = enforceBuiltinArgCount(builtin, len(args))
        if error:
            return error
        return func(cmd_ctx, args)
    raise Exception("TODO: expand_type {}".format(builtin.expand_type))

def runAssign(cmd_ctx: CommandContext, nodes: List[parse.Node]) -> Union[Error,ExitCode,UnknownExitCode]:
    assert(len(nodes) >= 2)
    assert(isinstance(nodes[1], parse.NodeAssign))
    if len(nodes) != 3:
        # should be a syntax error
        return SemanticError("expected 1 argument after '=' but got {}".format(len(nodes) - 2))
    args: List[bytes] = []
    varname = expandNode(cmd_ctx, nodes[0], ExpandNodeErrorContext())
    if isinstance(varname, Error):
        return varname
    if isinstance(varname, UnknownString):
        # this allow stitch to check more things like undefined variables and type errors with
        # user variables at verification time
        return SemanticError("for now, stitch requires variable names to be known at verification time")
    elif not isinstance(varname, String):
        return SemanticError("expected a String before '=' but got {}".format(varname.userTypeDescriptor()))
    value = expandNode(cmd_ctx, nodes[2], ExpandNodeErrorContext())
    if isinstance(value, Error):
        return value
    is_unknown_value = False
    if isinstance(value, UnknownString) or isinstance(value, UnknownBool) or isinstance(value, UnknownArray):
        is_unknown_value = True
    elif (not isinstance(value, String)) and (not isinstance(value, Bool)) and (not isinstance(value, Array)):
        return SemanticError("expected a String, Bool or Array after '=' but got {}".format(value.userTypeDescriptor()))

    cmd_ctx.var_map[varname.value] = value
    return UNKNOWN_EXIT_CODE if is_unknown_value else ExitCode(0)

def runCommandNodes(cmd_ctx: CommandContext, nodes: List[parse.Node]) -> Union[Error,Bool,ExitCode,UnknownBool,UnknownExitCode]:
    assert(len(nodes) > 0)
    # handle @end if we are disabled
    if not cmd_ctx.script.blockStack[-1].enabled:
        first = nodes[0]
        if isinstance(first, parse.NodeVariable) and (first.id == b"end"):
            if len(nodes) > 1:
                return SemanticError("'@end' does not accept any arguments")
            error = cmd_ctx.script.popBlock()
            if error:
                return error
        return ExitCode(0)

    # TODO: maybe enable printing this in verification_mode to assist
    #       in triaging SemanticErrors
    if not cmd_ctx.script.verification_mode and cmd_ctx.builtin_prefix_count == 0:
        # todo: is ("+" * (depth+1)) too inneficient?
        message = "{} {}".format("+" * (cmd_ctx.depth+1), " ".join([cmd_ctx.script.src[n.pos:n.end].decode('utf8') for n in nodes]))
        # NOTE: ignore capture_stdout, just always print to console for now
        eprint(message)

    # NOTE: this part should have been done by parser
    if len(nodes) >= 2 and isinstance(nodes[1], parse.NodeAssign):
        return runAssign(cmd_ctx, nodes)

    result = expandNodes(cmd_ctx, nodes)
    if isinstance(result, Error):
        return result
    if isinstance(result, ExpandNodes.Builtin):
        return runBuiltin(cmd_ctx, result.builtin, nodes[1:])
    if isinstance(result, ExpandNodes.BinaryExp):
        return runBinaryExpression(cmd_ctx, nodes, result.first, result.op)
    if isinstance(result, ExpandNodes.Bool):
        return SemanticError("unhandled Bool")
    assert(isinstance(result, ExpandNodes.ExternalProgram))
    return runExternalProgram(cmd_ctx.script.verification_mode, cmd_ctx.capture.stdout, cmd_ctx.capture.stderr, result.args)

def nodesToArgs(cmd_ctx: CommandContext, nodes: List[parse.Node], args: List[bytes], error_ctx: ExpandNodeErrorContext, start: int = 0) -> Optional[Error]:
    for i, node in enumerate(nodes[start:], start=start):
        obj = expandNode(cmd_ctx, node, error_ctx)
        if isinstance(obj, Error):
            return obj
        error = objectToArgs(obj, args)
        if error:
            return error
    return None

UNKNOWN_ARG_STRING = b"<UNKNOWN_ARG_STRING>"

def objectToArgs(obj: StitchObject, args: List[bytes]) -> Optional[Error]:
    assert(not isinstance(obj, Error))
    if isinstance(obj, String):
        args.append(obj.value)
        return None
    if isinstance(obj, Array):
        args.extend(obj.elements)
        return None
    if isinstance(obj, UnknownString):
        args.append(UNKNOWN_ARG_STRING)
        return None
    if isinstance(obj, Bool) or isinstance(obj, UnknownBool):
        return CannotCoerceToStringError("Bool")
    if isinstance(obj, BinaryOperator):
        return SemanticError("unexpected '{}'".format(obj))
    if isinstance(obj, Builtin):
        return CannotCoerceToStringError("Builtin '{}'".format(obj))

    return SemanticError("TODO: implement objectToArgs for type {} ({})".format(type(obj), obj))

def runExternalProgram(verification_mode: bool, stdout_handler: DataHandler,
                       stderr_handler: DataHandler, args: List[bytes]) -> Union[Error,ExitCode,UnknownExitCode]:
    if len(args) == 0:
        # NOTE: this can happen if the user executed an expanded empty array
        #       what should we do in this case?
        # NOTE: this is actually a RuntimeError because we currently don't detect
        #       this until the array is expanded at runtime
        return SemanticError("got a command with no arguments, what should the language do here?")

    if verification_mode:
        return UNKNOWN_EXIT_CODE

    prog = args[0]

    if not b"/" in prog:
        prog_filename = which(prog)
        if not prog_filename:
            return MissingProgramError(prog)
        # TODO: on linux we can specify a program file, and not modify args
        args[0] = prog_filename

    stdout = None if isinstance(stdout_handler, ConsolePrinter) else subprocess.PIPE
    stderr = None if isinstance(stderr_handler, ConsolePrinter) else subprocess.PIPE
    result = subprocess.run(args, stdout=stdout, stderr=stderr)
    if not isinstance(stdout_handler, ConsolePrinter):
        stdout_handler.handle(result.stdout)
    if not isinstance(stderr_handler, ConsolePrinter):
        stderr_handler.handle(result.stderr)
    return ExitCode(result.returncode)

def tryLookupCommandVar(cmd_ctx: CommandContext, name: bytes):
    ctx: Optional[CommandContext] = cmd_ctx
    while ctx:
        obj = ctx.var_map.get(name)
        if obj:
            return obj
        ctx = ctx.parent

def tryLookupUserVar(cmd_ctx: CommandContext, name: bytes) -> Optional[StitchObject]:
    cmd_obj = tryLookupCommandVar(cmd_ctx, name)
    if cmd_obj:
        return cmd_obj
    script_obj = cmd_ctx.script.var_map.get(name)
    if script_obj:
        return script_obj
    return None

def tryLookupBuiltinVar(script_ctx: ScriptContext, name: bytes) -> Optional[StitchObject]:
    script_obj = script_ctx.script_specific_builtin_objects.get(name)
    if script_obj:
        return script_obj
    global_obj = builtin_objects.get(name)
    if global_obj:
        return global_obj
    return None

def stdoutOnlyHandler(stdout: bytes, multiline: bool):
    assert(isinstance(stdout, bytes))
    if multiline:
        return String(stdout)
    lines = stdout.splitlines()
    if len(lines) == 0:
        return String(b"")
    elif len(lines) == 1:
        return String(stripNewline(lines[0]))
    return UnexpectedMultilineError(stdout)

def isCaptured(s):
    return s != None

def combineRunResultWithOutputs(cmd_ctx, result):
    if isinstance(result, Error):
        return result

    stdout = None
    stderr = None
    if cmd_ctx.parent:
        if cmd_ctx.capture.stdout != cmd_ctx.parent.capture.stdout:
            assert(isinstance(cmd_ctx.capture.stdout, StringBuilder))
            stdout = cmd_ctx.capture.stdout.output
            assert(type(stdout) == bytes)
        if cmd_ctx.capture.stderr != cmd_ctx.parent.capture.stderr:
            assert(isinstance(cmd_ctx.capture.stderr, StringBuilder))
            stderr = cmd_ctx.capture.stderr.output
            assert(type(stderr) == bytes)

    exitcode = None
    if isinstance(result, ExitCode):
        is_unknown = False
        exitcode = result.value
    elif isinstance(result, UnknownExitCode):
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

    if (isinstance(result, Bool) or isinstance(result, Array)
        or isinstance(result, UnknownBool) or isinstance(result, UnknownArray)):

        if cmd_ctx.capture.exitcode or isCaptured(stdout) or isCaptured(stderr):
            raise Exception("codebug? ec={} stdout={} stderr={}".format(cmd_ctx.capture.exitcode, isCaptured(stdout), isCaptured(stderr)))
        return result

    raise Exception("expected an ExitCode, Bool or Array but got {}".format(result.userTypeDescriptor()))


# returns an array of strings and builtin objects
def expandNode(cmd_ctx: CommandContext, node: parse.Node, error_ctx: ExpandNodeErrorContext) -> Union[Error,StitchObject]:
    if isinstance(node, parse.NodeToken):
        return String(node.s)

    if isinstance(node, parse.NodeVariable):
        if node.is_at:
            obj = tryLookupBuiltinVar(cmd_ctx.script, node.id)
        else:
            obj = tryLookupUserVar(cmd_ctx, node.id)
        if not obj:
            return SemanticError("'{}' is undefined".format(cmd_ctx.script.src[node.pos:node.end].decode('utf8')))
        return obj

    if isinstance(node, parse.NodeAssign):
        if error_ctx.inside_multiple:
            return SemanticError("'=' requires space separation")
        return SemanticError("unexpected '='")

    if isinstance(node, parse.NodeInlineCommand):
        inline_cmd_ctx = cmd_ctx.createChild(node.is_binary_expr)
        result = runCommandNodes(inline_cmd_ctx, node.nodes)
        return combineRunResultWithOutputs(inline_cmd_ctx, result)

    if isinstance(node, parse.NodeMultiple):
        args: List[bytes] = []
        error = nodesToArgs(cmd_ctx, node.nodes, args, ExpandNodeErrorContext(inside_multiple=True))
        if error:
            return error
        return String(b"".join(args))

    raise Exception("codebug, unhandled node type {}".format(type(node)))

def disableCaptureModifiers(cmd_ctx: CommandContext, error_context_str: str):
    if cmd_ctx.capture.exitcode:
        return SemanticError("@exitcode is not compatible with {}".format(error_context_str))
    if cmd_ctx.parent is not None and cmd_ctx.capture.stderr is not cmd_ctx.parent.capture.stderr:
        return SemanticError("@stderr is not compatible with {}".format(error_context_str))
    if cmd_ctx.parent is not None and cmd_ctx.capture.stdout is not cmd_ctx.parent.capture.stdout:
        assert(isinstance(cmd_ctx.capture.stdout, StringBuilder))
        assert(len(cmd_ctx.capture.stdout.output) == 0)
        cmd_ctx.capture.stdout = cmd_ctx.parent.capture.stdout
    return None

def runBinaryExpression(cmd_ctx: CommandContext, nodes: List[parse.Node], first_obj: StitchObject, op: BinaryOperator):
    if cmd_ctx.ambiguous_op:
        return SemanticError("got binary expression inside ambiguous operator '{}', wrap inside (..parenthesis..)".format(cmd_ctx.ambiguous_op))

    error = disableCaptureModifiers(cmd_ctx, "binary expressions")
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

        operand_result = expandNode(cmd_ctx, nodes[index+1], ExpandNodeErrorContext())
        if isinstance(operand_result, Error):
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
        if not isinstance(next_op, parse.NodeVariable):
            if isinstance(next_op, parse.NodeToken):
                return SemanticError("expected '{}' operator but got token '{}'; commands must be wrapped with (...)".format(op, next_op.s.decode('ascii')))
            return SemanticError("TODO: good error message for node that was expected to be an operand: {}".format(next_op))
        if next_op.id != op.name:
            return SemanticError("'{}' and '@{}' cannot be chained".format(op, next_op.id.decode('ascii')))

def runScript(script_ctx: ScriptContext, stdout_handler: DataHandler, stderr_handler: DataHandler) -> Union[Error,ExitCode]:
    pos = 0
    while pos < len(script_ctx.src):
        nodes, is_binary_expr, end = parse.parseCommand(script_ctx.src, pos, script_ctx.allstringliterals)
        assert(end > pos)
        pos = end

        if len(nodes) == 0:
            continue

        # Note: it seems like it might be better to just print the
        #       line in its source form rather than the expanded form
        #       definitely should have an option for this
        #if script_ctx.print_trace:
        #    message = "+ {}".format(line)
        #    if capture_stdout:
        #        output += message + "\n"
        #    else:
        #        print(message)
        result = runCommandNodes(CommandContext(
            script_ctx,
            is_binary_expr,
            parent=None,
            depth=0,
            capture=Capture(False, stdout_handler, stderr_handler),
            builtin_prefix_count=0,
            ambiguous_op=None
        ), nodes)
        if isinstance(result, Error):
            return result
        if isinstance(result, Bool):
            return SemanticError("unhandled Bool whose value is {}".format(result.value))
        if isinstance(result, UnknownBool):
            return SemanticError("unhandled Bool".format())
        if isinstance(result, ExitCode):
            if result.value != 0:
                return result
        else:
            assert(script_ctx.verification_mode and (
                isinstance(result, UnknownBool) or
                isinstance(result, UnknownExitCode)
            ))

    if len(script_ctx.blockStack) != 1:
        return SemanticError("need more '@end'")
    return ExitCode(0)

def normalizeFilename(filename):
    # TODO: implement this
    return filename

def runFile(global_ctx: GlobalContext, full_filename: Optional[bytes], src: bytes,
            stdout_handler: DataHandler, stderr_handler: DataHandler) -> Union[Error,ExitCode]:
    assert((full_filename is None) or isinstance(full_filename, bytes))
    assert(isinstance(src, bytes))

    full_filename_str = full_filename.decode('utf8') if full_filename else "<the -c command>"

    try:
        output = ""

        if global_ctx.doverify:
            normalized_filename = normalizeFilename(full_filename) if full_filename else None
            if not normalized_filename or (not normalized_filename in global_ctx.verify_started):
                if normalized_filename:
                    global_ctx.verify_started.add(normalized_filename)
                eprint("stitch: DEBUG: verifying '{}'".format(full_filename_str))
                stdout_builder = StringBuilder()
                stderr_builder = StringBuilder()
                script_ctx = ScriptContext(global_ctx, full_filename, src, verification_mode=True)
                result = runScript(script_ctx, stdout_builder, stderr_builder)
                if isinstance(result, Error):
                    ## This can happen if the arguments of an assert are known at "verification time"
                    ## i.e. @assert @true
                    #if isinstance(result, AssertError):
                    #    eprint("stitch: {}: AssertError: {}".format(full_filename_str, result.src))
                    #else:
                    #    assert(isinstance(result, SemanticError))
                    #    eprint("stitch: {}: SemanticError: {}".format(full_filename_str, result.message))
                    # I think error will be reported by caller?
                    return result
                # I'm allowing things to print to stdout during verification mode
                # because this can allow other mechnisms to get "further" in verification
                if len(stdout_builder.output) > 0:
                    pass
                    #sys.exit("something printed to stdout during verification mode???")
                if len(stderr_builder.output) > 0:
                    pass
                    #sys.exit("something printed to stderr during verification mode???")
                eprint("stitch: DEBUG: verification done on '{}'".format(full_filename_str))

        script_ctx = ScriptContext(global_ctx, full_filename, src, verification_mode=False)
        return runScript(script_ctx, stdout_handler, stderr_handler)
    except lex.SyntaxError as lex_syntax_err:
        return SyntaxError(full_filename_str, src, lex_syntax_err)

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

def cleanSandboxDir(sandbox_path: str, before_execution: bool):
    assert(sandbox_path == os.getcwd())
    clean_count = 0
    for entry_base in os.listdir(sandbox_path):
        entry = os.path.join(sandbox_path, entry_base)
        if os.path.isfile(entry):
            if before_execution:
                eprint("stitch: DEBUG: cleaning sandbox file '{}'".format(entry))
            else:
                eprint("stitch: Error: this script must be using relative filenames because the sandbox has this file '{}'".format(entry))
            clean_count += 1
            os.remove(entry)
        else:
            if not before_execution:
                eprint("stitch: Error: this script must be using relative filenames because the sandbox has this directory '{}'".format(entry))
            clean_count += cleanSandboxDir(entry, before_execution)
            os.rmdir(entry)
            clean_count += 1
    return clean_count

# try to get rid of CWD state by going to a temporary readonly directory
def sandboxCallerWorkdir() -> Tuple[str,str]:
    if os.name == "nt":
        temp_dir = os.getenv("TEMP")
        if not temp_dir:
            sys.exit("stitch: Error: environment variable %TEMP% is not set")
    else:
        temp_dir = "/tmp"
    sandbox_path = os.path.join(temp_dir, "stitch-sandbox")
    callerworkdir = resolveCallerWorkdir(sandbox_path)

    if not os.path.exists(sandbox_path):
        os.mkdir(sandbox_path)
    os.chdir(sandbox_path)
    cleanSandboxDir(sandbox_path, before_execution=True)

    return sandbox_path, callerworkdir

def main():
    cmd_args = sys.argv[1:]
    if len(cmd_args) == 0:
        sys.exit("Usage: stitch FILE")
        sys.exit("       stitch -c COMMAND (this is only 1 argument)")

    doverify = True

    first_arg = cmd_args[0]
    cmd_args = cmd_args[1:]
    if first_arg == "-c":
        if len(cmd_args) != 1:
            sys.exit("Error: -c requires 1 argument but got {}".format(len(cmd_args)))
        filename_str = "<the -c command>"
        full_filename = None
        src = cmd_args[0].encode('utf8')
    else:
        if len(cmd_args) > 0:
            sys.exit("Error: too many command-line arguments")
        filename_str = first_arg
        full_filename = os.path.abspath(first_arg).encode('utf8')
        with open(first_arg, "rb") as file:
            # TODO: try mmap if it is supported
            # NOTE: this will require using bytes instead of strings in Python
            src = file.read()

    sandbox_path, callerworkdir = sandboxCallerWorkdir()
    global_ctx = GlobalContext(doverify, callerworkdir.encode('utf8'))
    result = runFile(global_ctx, full_filename, src, CONSOLE_PRINTER, CONSOLE_PRINTER)
    sandbox_clean_count = cleanSandboxDir(sandbox_path, before_execution=False)
    if isinstance(result, Error):
        prefix = "Semantic" if isinstance(result, SemanticError) else ""
        eprint("stitch: {}: {}Error: {}".format(filename_str, prefix, result.message))
        sys.exit(1)

    assert(isinstance(result, ExitCode))
    if result.value != 0:
        sys.exit("stitch: Error: the last progam exited with code {}".format(result.value))

    if sandbox_clean_count > 0:
        sys.exit("stitch: Error: {} file(s) were cleaned from the sandbox".format(sandbox_clean_count))

if __name__ == "__main__":
    main()
