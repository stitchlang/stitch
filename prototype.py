#!/usr/bin/env python3
import sys
import os
import threading
from threading import Thread
import subprocess
import re
from abc import ABC, abstractmethod
from enum import Enum
from typing import List, Dict, Set, Union, Tuple, Optional

import lex
import parse
from parse import BinaryOpKind

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
    CommandKindResult = 1
    Objects = 2
    Strings = 3
class BuiltinReturnType(Enum):
    ExitCode = 0
    Bool = 1
    Array = 2
    NoReturn = 3

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

class NoReturn(StitchObject):
    @staticmethod
    def userTypeDescriptor():
        return "NoReturn"
NO_RETURN = NoReturn()

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
    def __init__(self, min_count: int, max_count: Optional[int]):
        Unknown.__init__(self, Array)
        self.min_count = min_count
        self.max_count = max_count
    @staticmethod
    def userTypeDescriptor():
        return "Array"
    def empty(self):
        assert((self.max_count == None) or (self.max_count >= self.min_count))
        return self.max_count == 0
    def addScalar(self, count: int):
        self.min_count += count
        if self.max_count is not None:
            self.max_count += count

class BinaryEvaluator:
    def __init__(self, kind: BinaryOpKind):
        self.kind = kind
    def __repr__(self):
        return parse.binaryOpUserString(self.kind)
    @abstractmethod
    def initialValue(self, operand: StitchObject):
        pass
    @abstractmethod
    def apply(self, verification_mode: bool, left: Bool, right: StitchObject):
        pass
class ChainableBinaryEvaluator(BinaryEvaluator):
    def __init__(self, kind: BinaryOpKind):
        BinaryEvaluator.__init__(self, kind)
    @abstractmethod
    def shortcircuit(self, result: Bool):
        return not result.value
class AndEvaluator(ChainableBinaryEvaluator):
    def __init__(self):
        ChainableBinaryEvaluator.__init__(self, BinaryOpKind.AND)
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
class OrEvaluator(ChainableBinaryEvaluator):
    def __init__(self):
        ChainableBinaryEvaluator.__init__(self, BinaryOpKind.OR)
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
class EqEvaluator(BinaryEvaluator):
    def __init__(self):
        BinaryEvaluator.__init__(self, BinaryOpKind.EQ)
    def initialValue(self, operand):
        if isinstance(operand, String):
            return operand
        if isinstance(operand, UnknownString):
            return UNKNOWN_STRING
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
class CompareEvaluator(BinaryEvaluator):
    def __init__(self, kind: BinaryOpKind, func):
        BinaryEvaluator.__init__(self, kind)
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
    def __init__(self, exitcode: int, stdout: Union[None,UnknownString,bytes], stderr: Union[None,UnknownString,bytes]):
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
    def __init__(self, message: str):
        assert(isinstance(message, str))
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
    def __init__(self, exitcode: int, stdout: Optional[bytes], stderr: Optional[bytes]):
        assert(exitcode != 0)
        Error.__init__(self, "command failed with exit code {}".format(exitcode))
        self.exitcode = exitcode
        self.stdout = stdout
        self.stderr = stderr
class CommandWithNoArgumentsError(Error):
    def __init__(self):
        Error.__init__(self, "got a command with no arguments")
class MissingProgramError(Error):
    def __init__(self, filename: bytes):
        assert(isinstance(filename, bytes))
        Error.__init__(self, "unable to find program '{}' in PATH".format(filename.decode('utf8')))
class MissingStitchScriptError(Error):
    def __init__(self, prog_str: str):
        Error.__init__(self, "stitch script '{}' does not exist".format(prog_str))
class UnexpectedMultilineError(Error):
    def __init__(self, stdout):
        Error.__init__(self, "missing '@multiline', got this multiline output\n----\n{}----\n".format(stdout.decode('utf8')))
class UndefinedEnvironmentVariableError(Error):
    def __init__(self, name_str: str):
        Error.__init__(self, "undefined environment variable '{}'".format(name_str))
        self.name_str = name_str
class UnreachableError(Error):
    def __init__(self):
        super().__init__("reached @unreachable")

# These ExitCode and UnknownExitCode types are currently only used internally
class ExitCode:
    def __init__(self, value: int):
        self.value = value
        self.multiline = False
class UnknownExitCode:
    def __init__(self):
        self.multiline = False

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

class Writer(ABC):
    @abstractmethod
    def handle(self, s: bytes):
        pass
class RuntimeWriter(Writer):
    pass
class StringBuilder(RuntimeWriter):
    def __init__(self):
        self.output = b""
    @staticmethod
    def descriptor():
        return "capture"
    def handle(self, s: bytes):
        assert(type(s) == bytes)
        if len(s) > 0:
            self.output += s
class ConsoleWriter(RuntimeWriter):
    @staticmethod
    def descriptor():
        return "console"
    def handle(self, s: bytes):
        assert(type(s) == bytes)
        if len(s) > 0:
            sys.stdout.buffer.write(s)
CONSOLE_WRITER = ConsoleWriter()

class IncompletePipeWriter(RuntimeWriter):
    @staticmethod
    def descriptor():
        return "incomplete-pipe"
    def handle(self, s: bytes):
        raise Exception("codebug: cannot write to an incomplete-pipe")
INCOMPLETE_PIPE_WRITER = IncompletePipeWriter()

class PipeWriter(RuntimeWriter):
    @abstractmethod
    def getPipeReader(self) -> "Reader":
        pass
    @abstractmethod
    def close(self):
        pass

class PipeWriterToBuiltin(PipeWriter):
    def __init__(self):
        self.lock: threading.Lock = threading.Lock()
        self.data_event: threading.Event = threading.Event()
        self.data_builder = StringBuilder()
        self.closed = False
    @staticmethod
    def descriptor():
        return "pipe-writer-to-builtin"
    def handle(self, s: bytes):
        with self.lock:
            assert(not self.closed)
            self.data_builder.handle(s)
            self.data_event.set()
    def getPipeReader(self) -> "PipeReaderForBuiltin":
        return PipeReaderForBuiltin(self)
    def close(self):
        with self.lock:
            assert(not self.closed)
            self.closed = True
            self.data_event.set()

class PipeWriterToExternalProgram(PipeWriter):
    def __init__(self, popen: subprocess.Popen):
        self.popen = popen
    @staticmethod
    def descriptor():
        return "pipe-writer-to-program"
    def handle(self, s: bytes):
        #print("DEBUG: PipeWriterToExternalProgram: writing {} bytes...".format(len(s)))
        self.popen.stdin.write(s)
    def getPipeReader(self) -> "PipeReaderForExternalProgram":
        return PipeReaderForExternalProgram(self.popen)
    def close(self):
        #print("DEBUG: PipeWriterToExternalProgram: close")
        self.popen.stdin.close()

class VerifyWriter(Writer):
    def __init__(self, builder: Optional[StringBuilder], for_pipe: bool):
        self.builder: Optional[StringBuilder] = builder
        self.for_pipe = for_pipe
    @staticmethod
    def descriptor():
        return "verify-for-pipe" if self.for_pipe else "verify"
    def handle(self, s: bytes):
        if self.builder is not None:
            self.builder.handle(s)
    def handleUnknownData(self):
        self.builder = None

def newCaptureWriter(verification_mode: bool) -> Writer:
    return VerifyWriter(StringBuilder(), for_pipe=False) if verification_mode else StringBuilder()

class Reader(ABC):
    @abstractmethod
    def read(self) -> Optional[bytes]:
        pass

class StringReader(Reader):
    def __init__(self, s: bytes):
        self.s: Optional[bytes] = s
    @staticmethod
    def descriptor():
        return "string"
    def read(self) -> Optional[bytes]:
        result = self.s
        self.s = None
        return result

class ConsoleReader(Reader):
    @staticmethod
    def descriptor():
        return "console"
    def read(self) -> Optional[bytes]:
        result = sys.stdin.buffer.read()
        if len(result) == 0:
            return None
        return result
CONSOLE_READER = ConsoleReader()

class IncompletePipeReader(Reader):
    @staticmethod
    def descriptor():
        return "incomplete-pipe"
    def read(self) -> Optional[bytes]:
        raise Exception("codebug: cannot write from an incomplete-pipe")
INCOMPLETE_PIPE_READER = IncompletePipeReader()

class PipeReaderForBuiltin(Reader):
    def __init__(self, writer: PipeWriterToBuiltin):
        self.writer = writer
    @staticmethod
    def descriptor():
        return "pipe-reader-for-builtin"
    def read(self) -> Optional[bytes]:
        while True:
            with self.writer.lock:
                if self.writer.closed:
                    break
            self.writer.data_event.wait()
            with self.writer.lock:
                output = self.writer.data_builder.output
                if len(output) > 0:
                    self.writer.data_builder.output = b""
                    return output

        # no need to lock because writer is closed
        output = self.writer.data_builder.output
        if len(output) > 0:
            self.writer.data_builder.output = b""
            return output
        return None

class PipeReaderForExternalProgram(Reader):
    def __init__(self, popen: subprocess.Popen):
        self.popen = popen
    @staticmethod
    def descriptor():
        return "pipe-reader-for-program"
    def read(self) -> Optional[bytes]:
        #print("DEBUG: PipeReaderForExternalProgram: read...")
        # TODO: handle stderr as well
        result = self.popen.stdout.read()
        #print("DEBUG: PipeReaderForExternalProgram: read returned {}".format(result))
        assert(isinstance(result, bytes))
        if len(result) == 0:
            return None
        return result

class Capture:
    def __init__(self, exitcode: bool, stdout: Writer, stderr: Writer, stdin: Optional[Reader]):
        self.exitcode = exitcode
        self.stdout = stdout
        self.stderr = stderr
        self.stdin = stdin
    def __repr__(self):
        return "Capture(exitcode={},stdout={},stderr={},stdin={})".format(
            self.exitcode, self.stdout.descriptor(), self.stderr.descriptor(),
            "verify" if (self.stdin is None) else self.stdin.descriptor())

class CommandContext:
    def __init__(
            self,
            script: ScriptContext,
            parent: Optional['CommandContext'],
            depth: int,
            capture: Capture,
            builtin_prefix_count: int,
            ambiguous_op: Optional[str],
            var_map: Dict[bytes,StitchObject] = {}
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
        stdout = newCaptureWriter(self.script.verification_mode)
        return type(self)(self.script, self, self.depth+1,
                          Capture(exitcode=False,stdout=stdout,stderr=self.capture.stderr,stdin=self.capture.stdin),
                          builtin_prefix_count=0, ambiguous_op=None)
    def createPipeChild(self) -> 'CommandContext':
        return type(self)(self.script, self, self.depth+1,
                          Capture(exitcode=False,
                                  stdout=INCOMPLETE_PIPE_WRITER,
                                  stderr=self.capture.stderr,
                                  stdin=INCOMPLETE_PIPE_READER),
                          builtin_prefix_count=0, ambiguous_op=None)
    def nextBuiltin(self, ambiguous_op: Optional[str]) -> 'CommandContext':
        return type(self)(self.script, self.parent, self.depth, self.capture,
                          self.builtin_prefix_count + 1, ambiguous_op, self.var_map)

def assertArgCount(expected: int, known_arg_count: int, unknown_args: UnknownArray):
    if unknown_args.min_count == unknown_args.max_count:
        assert(expected == known_arg_count + unknown_args.min_count)
    else:
        assert(known_arg_count + unknown_args.min_count <= expected)
        if unknown_args.max_count:
            assert(known_arg_count + unknown_args.max_count >= expected)

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
    def echo(cmd_ctx: CommandContext, args: List[bytes], unknown_args: UnknownArray):
        if cmd_ctx.script.verification_mode:
            if not unknown_args.empty():
                assert(isinstance(cmd_ctx.capture.stdout, VerifyWriter))
                cmd_ctx.capture.stdout.handleUnknownData()
                return UnknownExitCode()
        else:
            assert(unknown_args.empty())
        output = b" ".join(args)
        if len(output) > 0:
            cmd_ctx.capture.stdout.handle(output + b"\n")
        return ExitCode(0)
    @staticmethod
    def cat(cmd_ctx: CommandContext, args: List[bytes], unknown_args: UnknownArray):
        if cmd_ctx.script.verification_mode:
            if not unknown_args.empty():
                assert(isinstance(cmd_ctx.capture.stdout, VerifyWriter))
                cmd_ctx.capture.stdout.handleUnknownData()
                return UnknownExitCode()
        else:
            assert(unknown_args.empty())

        if len(args) == 0:
            if cmd_ctx.capture.stdin is None:
                assert(cmd_ctx.script.verification_mode)
                assert(isinstance(cmd_ctx.capture.stdout, VerifyWriter))
                cmd_ctx.capture.stdout.handleUnknownData()
                return ExitCode(0)
            while True:
                data = cmd_ctx.capture.stdin.read()
                if data is None:
                    return ExitCode(0)
                cmd_ctx.capture.stdout.handle(data)

        if len(args) == 1:
            filename = args[0]
            if cmd_ctx.script.verification_mode:
                assert(isinstance(cmd_ctx.capture.stdout, VerifyWriter))
                cmd_ctx.capture.stdout.handleUnknownData()
                return ExitCode(0)
            # TODO: try various methods to cat such as linux 'sendfile"
            try:
                with open(filename, "rb") as f:
                    cmd_ctx.capture.stdout.handle(f.read())
            except FileNotFoundError:
                cmd_ctx.capture.stderr.handle("error: file not found '{}'".format(filename.decode('utf8')).encode('utf8'))
                return ExitCode(1)
            return ExitCode(0)

        return SemanticError("'@cat' takes 0 or 1 arguments but got {}".format(len(args)))
    @staticmethod
    def stdin2file(cmd_ctx: CommandContext, args: List[bytes], unknown_args: UnknownArray):
        if cmd_ctx.script.verification_mode:
            assertArgCount(1, len(args), unknown_args)
            return UnknownExitCode()
        assert(len(args) == 1)

        file = None
        try:
            try:
                file = open(args[0], "wb")
            except FileNotFoundError:
                cmd_ctx.capture.stderr.handle("error: file not found '{}'".format(args[0].decode('utf8')).encode('utf8'))
                return ExitCode(1)
            if cmd_ctx.capture.stdin:
                while True:
                    data = cmd_ctx.capture.stdin.read()
                    if data is None:
                        return ExitCode(0)
                    file.write(data)
        finally:
            if file is not None:
                file.close()

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
    def setenv(cmd_ctx: CommandContext, args: List[bytes], unknown_args: UnknownArray):
        if cmd_ctx.script.verification_mode:
            assertArgCount(2, len(args), unknown_args)
            if not unknown_args.empty():
                return UnknownExitCode()
        else:
            assert(unknown_args.empty())
        assert(len(args) == 2)
        name = args[0]
        value = args[1]
        if not cmd_ctx.script.verification_mode:
            os.environ[name.decode('utf8')] = value.decode('utf8')
        return ExitCode(0)
    @staticmethod
    def unsetenv(cmd_ctx: CommandContext, args: List[bytes], unknown_args: UnknownArray):
        if cmd_ctx.script.verification_mode:
            assertArgCount(1, len(args), unknown_args)
            return UnknownExitCode()
        else:
            assert(unknown_args.empty())
        assert(len(args) == 1)
        name = args[0]
        name_str = name.decode('utf8')
        if not name_str in os.environ:
            return UndefinedEnvironmentVariableError(name_str)
        del os.environ[name_str]
        return ExitCode(0)
    @staticmethod
    def env(cmd_ctx: CommandContext, args: List[bytes], unknown_args: UnknownArray):
        if cmd_ctx.script.verification_mode:
            assertArgCount(1, len(args), unknown_args)
            assert(isinstance(cmd_ctx.capture.stdout, VerifyWriter))
            cmd_ctx.capture.stdout.handleUnknownData()
            return UnknownExitCode()

        assert(unknown_args.empty())
        assert(len(args) == 1)
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
            assert(isinstance(cmd_ctx.capture.stdout, VerifyWriter))
            cmd_ctx.capture.stdout.handleUnknownData()
            return UnknownExitCode()
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
    def isfile(cmd_ctx: CommandContext, args: List[bytes], unknown_args: UnknownArray):
        if cmd_ctx.script.verification_mode:
            assertArgCount(1, len(args), unknown_args)
            return UNKNOWN_BOOL
        assert(unknown_args.empty())
        assert(len(args) == 1)
        return BOOL_TRUE if os.path.isfile(args[0]) else BOOL_FALSE
    @staticmethod
    def isdir(cmd_ctx: CommandContext, args: List[bytes], unknown_args: UnknownArray):
        if cmd_ctx.script.verification_mode:
            assertArgCount(1, len(args), unknown_args)
            return UNKNOWN_BOOL
        assert(unknown_args.empty())
        assert(len(args) == 1)
        return BOOL_TRUE if os.path.isdir(args[0]) else BOOL_FALSE
    @staticmethod
    def haveprog(cmd_ctx: CommandContext, args: List[bytes], unknown_args: UnknownArray):
        if cmd_ctx.script.verification_mode:
            assertArgCount(1, len(args), unknown_args)
            return UNKNOWN_BOOL
        assert(unknown_args.empty())
        assert(len(args) == 1)
        return BOOL_TRUE if which(args[0]) else BOOL_FALSE

    @staticmethod
    def assert_(cmd_ctx: CommandContext, nodes: List[parse.Node]):
        result = expandNodesToBool(cmd_ctx.nextBuiltin(ambiguous_op=None), nodes, "@assert", False)
        if isinstance(result, Error):
            return result
        if isinstance(result, UnknownBool):
            return UnknownExitCode()
        assert(isinstance(result, Bool))
        if not result.value:
            return AssertError(nodes)
        return ExitCode(0)
    @staticmethod
    def unreachable(cmd_ctx: CommandContext, nodes: List[parse.Node]):
        if cmd_ctx.script.verification_mode:
            return UnknownExitCode()
        return UnreachableError()
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
        return UnknownExitCode()
    @staticmethod
    def end(cmd_ctx: CommandContext, nodes: List[parse.Node]):
        assert(len(nodes) == 0)
        error = cmd_ctx.script.popBlock()
        if error:
            return error
        return ExitCode(0)
    @staticmethod
    def call(cmd_ctx: CommandContext, args: List[bytes], unknown_args: UnknownArray):
        if cmd_ctx.script.verification_mode:
            if len(args) == 0 and unknown_args.empty():
                return SemanticError("@call requires at least one argument")
            return UnknownExitCode()
        assert(unknown_args.empty())
        if len(args) == 0:
            return SemanticError("@call requires at least one argument")
        program_file = args[0]
        if len(args) > 1:
            sys.exit("Error: @call with more than just a program not implemented")
        if cmd_ctx.script.verification_mode:
            return UnknownExitCode()
        # TODO: should I be caching files?
        try:
            with open(program_file, "rb") as file:
                # TODO: try mmap if it is supported
                src = file.read()
        except FileNotFoundError:
            return MissingStitchScriptError(program_file.decode('utf8'))
        result = runFile(cmd_ctx.script.global_ctx,
                         program_file,
                         src,
                         cmd_ctx.capture.stdout,
                         cmd_ctx.capture.stderr,
                         cmd_ctx.capture.stdin)
        if isinstance(result, Error):
            return result
        assert(isinstance(result, ExitCode))
        return result

    # arrays are limited to strings only right now, so we can expand all arguments to strings
    @staticmethod
    def array(cmd_ctx: CommandContext, args: List[bytes], unknown_args: UnknownArray):
        if cmd_ctx.script.verification_mode:
            if not unknown_args.empty():
                result = UnknownArray(unknown_args.min_count, unknown_args.max_count)
                result.addScalar(len(args))
                return result
        else:
            assert(unknown_args.empty())
        return Array(args)
    @staticmethod
    def lines2array(cmd_ctx: CommandContext, args: List[bytes], unknown_args: UnknownArray):
        if cmd_ctx.script.verification_mode:
            assertArgCount(1, len(args), unknown_args)
            if not unknown_args.empty():
                return UnknownArray(0, None)
        assert(len(args) == 1)
        return Array(args[0].splitlines())
    @staticmethod
    def exit(cmd_ctx: CommandContext, args: List[bytes], unknown_args: UnknownArray):
        if cmd_ctx.script.verification_mode:
            assertArgCount(1, len(args), unknown_args)
            if len(args) > 0:
                try:
                    int(args[0])
                except ValueError:
                    return SemanticError("@exit requires an integer but got '{}'".format(args[0].decode('utf8')))
            return NO_RETURN
        assert(len(args) == 1)
        sys.exit(int(args[0]))
    @staticmethod
    def getuid(cmd_ctx: CommandContext, nodes: List[parse.Node]):
        assert(len(nodes) == 0)
        if os.name == "nt":
            return SemanticError("@getuid not supported on Windows")
        if cmd_ctx.script.verification_mode:
            assert(isinstance(cmd_ctx.capture.stdout, VerifyWriter))
            cmd_ctx.capture.stdout.handleUnknownData()
            return UnknownExitCode()
        cmd_ctx.capture.stdout.handle(str(os.getuid()).encode('ascii'))
        return ExitCode(0)

def opInvalidTypeError(op: BinaryEvaluator, operand: StitchObject):
    #raise Exception("'{}' does not accept objects of type {}".format(op, operand.userTypeDescriptor()))
    return SemanticError("'{}' does not accept objects of type {}".format(op, operand.userTypeDescriptor()))

def operandToBool(op: BinaryEvaluator, operand: StitchObject) -> Union[Error,Bool,UnknownBool]:
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
    b"cat": Builtin("cat", BuiltinExpandType.Strings, BuiltinReturnType.ExitCode),
    b"stdin2file": Builtin("stdin2file", BuiltinExpandType.Strings, BuiltinReturnType.ExitCode, arg_count=1),
    b"settmp": Builtin("settmp", BuiltinExpandType.ParseNodes, BuiltinReturnType.ExitCode),
    b"multiline": Builtin("multiline", BuiltinExpandType.ParseNodes, BuiltinReturnType.ExitCode),
    b"exitcode": Builtin("exitcode", BuiltinExpandType.ParseNodes, BuiltinReturnType.Bool),
    b"call": Builtin("call", BuiltinExpandType.Strings, BuiltinReturnType.ExitCode),
    b"assert": Builtin("assert_", BuiltinExpandType.ParseNodes, BuiltinReturnType.ExitCode),
    b"unreachable": Builtin("unreachable", BuiltinExpandType.ParseNodes, BuiltinReturnType.ExitCode),
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
    b"array": Builtin("array", BuiltinExpandType.Strings, BuiltinReturnType.Array),
    b"lines2array": Builtin("lines2array", BuiltinExpandType.Strings, BuiltinReturnType.Array, arg_count=1),
    b"exit": Builtin("exit", BuiltinExpandType.Strings, BuiltinReturnType.NoReturn, arg_count=1),
    b"getuid": Builtin("getuid", BuiltinExpandType.ParseNodes, BuiltinReturnType.ExitCode, arg_count=0),
}

binary_evaluators = {
    BinaryOpKind.OR: OrEvaluator(),
    BinaryOpKind.AND: AndEvaluator(),
    BinaryOpKind.EQ: EqEvaluator(),
    BinaryOpKind.GT: CompareEvaluator(BinaryOpKind.GT, CompareOp.gt),
    BinaryOpKind.LT: CompareEvaluator(BinaryOpKind.LT, CompareOp.lt),
}

def which(name: bytes) -> Optional[bytes]:
    extensions = [b""] if (os.name != "nt") else [e.encode('utf8') for e in os.environ["PATHEXT"].split(";")]
    for path in os.environ["PATH"].split(os.pathsep):
        path_bytes = path.encode('utf8')
        for ext in extensions:
            filename = os.path.join(path_bytes, name + ext)
            if os.path.isfile(filename) and os.access(filename, os.X_OK):
                return filename
    return None

# Use to provide extra context for better error messages
class ExpandNodeErrorContext:
    def __init__(self, inside_multiple: bool = False):
        self.inside_multiple = inside_multiple

class CommandKind:
    pass
class CommandKinds:
    class Builtin(CommandKind):
        def __init__(self, builtin: Builtin):
            self.builtin = builtin
    class BinaryExp(CommandKind):
        def __init__(self, kind: BinaryOpKind):
            self.kind = kind
    class Bool(CommandKind):
        def __init__(self, value: Union[Bool,UnknownBool]):
            self.value = value
    class ExternalProgram(CommandKind):
        pass
    EXTERNAL_PROGRAM = ExternalProgram()

def expandIfBuiltinProgram(cmd_ctx: CommandContext, node: parse.Node) -> Optional[Builtin]:
    if isinstance(node, parse.NodeVariable):
        if node.is_at:
            obj = tryLookupBuiltinVar(cmd_ctx.script, node.id)
            if obj:
                # NOTE: currently we have some special builtins like @false, @true, @scriptdir
                #       that are not programs, so we special case them for now
                if isinstance(obj, Builtin):
                    return obj
    return None
def getCommandKind(cmd_ctx: CommandContext, nodes: List[parse.Node]) -> Union[Error,CommandKind]:
    assert(len(nodes) > 0)

    first_builtin_program = expandIfBuiltinProgram(cmd_ctx, nodes[0])
    if first_builtin_program:
        return CommandKinds.Builtin(first_builtin_program)

    if len(nodes) >= 2:
        second_node = nodes[1]
        if isinstance(second_node, parse.NodeBinaryOp):
            return CommandKinds.BinaryExp(second_node.kind)

    first_obj = expandNode(cmd_ctx, nodes[0], ExpandNodeErrorContext())
    # TODO: are there other special types to handle here?
    if isinstance(first_obj, Bool) or isinstance(first_obj, UnknownBool):
        if len(nodes) != 1:
            return SemanticError("unexpected Bool at the start of a command")
        return CommandKinds.Bool(first_obj)

    return CommandKinds.EXTERNAL_PROGRAM

def expandNodesToBool(cmd_ctx: CommandContext, nodes: List[parse.Node], builtin_name_str: str, allow_cmd_result: bool) -> Union[Error,Bool,UnknownBool]:
    if len(nodes) == 0:
        return SemanticError("{} requires at least 1 argument".format(builtin_name_str))

    cmd_kind = getCommandKind(cmd_ctx, nodes)
    if isinstance(cmd_kind, Error):
        return cmd_kind
    if isinstance(cmd_kind, CommandKinds.Bool):
        return cmd_kind.value
    if isinstance(cmd_kind, CommandKinds.BinaryExp):
        result = runBinaryExpression(cmd_ctx, nodes, cmd_kind.kind)
        if (isinstance(result, Error) or
            isinstance(result, Bool) or
            isinstance(result, UnknownBool)):
            return result
        if (isinstance(result, ExitCode) or
            isinstance(result, UnknownExitCode)):
            return SemanticError("{} expects a Bool but got an ExitCode".format(builtin_name_str))
        raise Exception("TODO: implement return type {} from runBinaryExpression".format(type(result).__name__))
    if isinstance(cmd_kind, CommandKinds.Builtin):
        next_result = runBuiltin(cmd_ctx, cmd_kind.builtin, nodes[1:])
        if isinstance(next_result, Bool) or isinstance(next_result, UnknownBool):
            return next_result
        # TODO: there are probably more types to handle here
    else:
        assert(isinstance(cmd_kind, CommandKinds.ExternalProgram))
        unknown_args = UnknownArray(0, 0)
        args = nodesToArgs(cmd_ctx, nodes, unknown_args, ExpandNodeErrorContext())
        if isinstance(args, Error):
            return args
        if cmd_ctx.script.verification_mode:
            next_result = UnknownExitCode()
        else:
            assert(isinstance(cmd_ctx.capture.stdout, RuntimeWriter))
            assert(isinstance(cmd_ctx.capture.stderr, RuntimeWriter))
            next_result = runExternalProgram(cmd_ctx.capture.stdout, cmd_ctx.capture.stderr, args)
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

def makeArgCountError(builtin: Builtin, actual: str) -> str:
    suffix = "" if (builtin.arg_count == 1) else "s"
    return "{} takes {} argument{} but got {}".format(
        builtin, builtin.arg_count, suffix, actual)

def enforceBuiltinArgCount(builtin: Builtin, known_arg_count: int, unknown_args: UnknownArray) -> Optional[Error]:
    if builtin.arg_count is not None:
        min_arg_count = known_arg_count + unknown_args.min_count
        if unknown_args.min_count == unknown_args.max_count:
            if builtin.arg_count != min_arg_count:
                return SemanticError(makeArgCountError(builtin, str(min_arg_count)))
        else:
            if min_arg_count > builtin.arg_count:
                return SemanticError(makeArgCountError(builtin, "at least {}".format(min_arg_count)))
            if unknown_args.max_count:
                max_arg_count = known_arg_count + unknown_args.max_count
                if max_arg_count < builtin.arg_count:
                    return SemanticError(makeArgCountError(builtin, "no more than {}".format(max_arg_count)))
    return None

def runBuiltin(cmd_ctx: CommandContext, builtin: Builtin, nodes: List[parse.Node]):
    if builtin.return_type != BuiltinReturnType.ExitCode:
        error = disableCaptureModifiers(cmd_ctx, builtin.atname_str)
        if error:
            return error

    func = getattr(BuiltinMethods, builtin.python_name_str)
    if builtin.expand_type == BuiltinExpandType.ParseNodes:
        error = enforceBuiltinArgCount(builtin, len(nodes), UnknownArray(0, 0))
        if error:
            return error
        return func(cmd_ctx, nodes)
    if builtin.expand_type == BuiltinExpandType.Strings:
        unknown_args = UnknownArray(0, 0)
        args = nodesToArgs(cmd_ctx, nodes, unknown_args, ExpandNodeErrorContext())
        if isinstance(args, Error):
            if isinstance(args, CannotCoerceToStringError):
                if builtin.arg_count is None:
                    requires = "Strings"
                elif builtin.arg_count == 1:
                    requires = "a String"
                else:
                    requires = "{} Strings".format(builtin.arg_count)
                return SemanticError("{} requires {} but got {}".format(builtin, requires, args.uncoercable_type))
            return args
        error = enforceBuiltinArgCount(builtin, len(args), unknown_args)
        if error:
            return error
        return func(cmd_ctx, args, unknown_args)
    raise Exception("TODO: expand_type {}".format(builtin.expand_type))

def runAssign(cmd_ctx: CommandContext, nodes: List[parse.Node]) -> Union[Error,ExitCode,UnknownExitCode]:
    assert(len(nodes) >= 2)
    assert(isinstance(nodes[1], parse.NodeBinaryOp))
    assert(nodes[1].kind == BinaryOpKind.ASSIGN)
    if cmd_ctx.builtin_prefix_count > 0:
        return SemanticError("unexpected '='") # better error?
    if cmd_ctx.depth > 0:
        return SemanticError("assignment '=' is not allowed inside an inline command")
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
        return SemanticError("variable names must be known at verification time")
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
    return UnknownExitCode() if is_unknown_value else ExitCode(0)

class PipeCommand(ABC):
    def __init__(self, cmd_ctx: CommandContext):
        self.cmd_ctx = cmd_ctx
    @abstractmethod
    def prepare(self) -> Optional[Error]:
        # NOTE: this is not called at verify time
        pass
    @abstractmethod
    def getPipeWriter(self) -> PipeWriter:
        pass
    @abstractmethod
    def run(self):
        pass
class PipeCommandBuiltin(PipeCommand):
    def __init__(self, cmd_ctx: CommandContext, builtin: Builtin, nodes: List[parse.Node]):
        super().__init__(cmd_ctx)
        self.builtin = builtin
        self.nodes = nodes
    def prepare(self) -> Optional[Error]:
        return None
    def getPipeWriter(self) -> PipeWriter:
        return PipeWriterToBuiltin()
    def run(self):
        return runBuiltin(self.cmd_ctx, self.builtin, self.nodes)
class PipeCommandExternalProgram(PipeCommand):
    def __init__(self, cmd_ctx: CommandContext, args: List[bytes], unknown_args: UnknownArray, pipe_stdout: bool):
        assert(len(args) > 0)
        super().__init__(cmd_ctx)
        self.args = args
        self.unknown_args = unknown_args
        self.pipe_stdout = pipe_stdout
        self.popen: Optional[subprocess.Popen] = None
    def prepare(self) -> Optional[Error]:
        assert(not self.cmd_ctx.script.verification_mode)
        assert(self.unknown_args.empty())
        assert(self.popen is None)
        if len(self.args) == 0:
            return CommandWithNoArgumentsError()
        prog = resolveExternalProgram(self.args[0])
        if isinstance(prog, Error):
            return prog
        stdout = subprocess.PIPE if self.pipe_stdout else None
        # TODO: stderr?
        args = [prog] + self.args[1:]
        #print(" ".join([a.decode('utf8') for a in args]))
        self.popen = subprocess.Popen(args, stdin=subprocess.PIPE, stdout=stdout)
        return None
    def getPipeWriter(self) -> PipeWriter:
        assert(self.popen is not None)
        return PipeWriterToExternalProgram(self.popen)
    def run(self):
        if self.cmd_ctx.script.verification_mode:
            return UnknownExitCode()
        if self.pipe_stdout:
            while True:
                result = self.popen.stdout.read()
                assert(isinstance(result, bytes))
                if len(result) == 0:
                    break
                self.cmd_ctx.capture.stdout.handle(result)
        result = self.popen.wait()
        assert(isinstance(result, int))
        return ExitCode(result)

class CommandThread(Thread):
    def __init__(self, pipe_cmd: PipeCommand):
        super().__init__()
        self.pipe_cmd = pipe_cmd
        self.return_value: Optional[Union[Error,Bool,ExitCode,UnknownBool,UnknownExitCode]] = None
    def run(self):
        self.return_value = self.pipe_cmd.run()
        if isinstance(self.pipe_cmd.cmd_ctx.capture.stdout, PipeWriter):
            self.pipe_cmd.cmd_ctx.capture.stdout.close()

ExpressionResult = Union[Error,ExitCode,Bool,String,Array,UnknownExitCode,UnknownBool,UnknownString,UnknownArray]

def runPipe(cmd_ctx: CommandContext, nodes: List[parse.Node]) -> ExpressionResult:
    assert(len(nodes) >= 2)
    assert(isinstance(nodes[1], parse.NodeBinaryOp))
    assert(nodes[1].kind == BinaryOpKind.PIPE)
    def pipeNodeError(node: parse.Node):
        return SemanticError("'@pipe' requires an inline command but got '{}'".format(node.userString(cmd_ctx.script.src)))
    if not isinstance(nodes[0], parse.NodeInlineCommand):
        return pipeNodeError(nodes[0])
    inline_cmd_nodes: List[parse.NodeInlineCommand] = [nodes[0]]
    i = 1
    while True:
        node = nodes[i+1]
        if not isinstance(node, parse.NodeInlineCommand):
            return pipeNodeError(node)
        inline_cmd_nodes.append(node)
        i += 2
        if len(nodes) == i:
            break
        next_op = nodes[i]
        if not isinstance(next_op, parse.NodeBinaryOp):
            return SemanticError("expected '@pipe' but got '{}'".format(next_op.userString(cmd_ctx.script.src)))

    if isinstance(cmd_ctx.capture.stdout, ConsoleWriter):
        pipe_last_command = False
    else:
        assert(isinstance(cmd_ctx.capture.stdout, StringBuilder) or isinstance(cmd_ctx.capture.stdout, VerifyWriter))
        pipe_last_command = True

    inline_cmds: List[PipeCommand] = []
    for i, inline_cmd_node in enumerate(inline_cmd_nodes):
        inline_cmd_ctx = cmd_ctx.createPipeChild()
        cmd_kind = getCommandKind(inline_cmd_ctx, inline_cmd_node.nodes)
        if isinstance(cmd_kind, Error):
            return cmd_kind
        if isinstance(cmd_kind, CommandKinds.BinaryExp):
            return SemanticError("unexpected binary expression within @pipe expression")
        if isinstance(cmd_kind, CommandKinds.Bool):
            return SemanticError("unexpected Bool within @pipe expression")

        if isinstance(cmd_kind, CommandKinds.Builtin):
            inline_cmds.append(PipeCommandBuiltin(inline_cmd_ctx, cmd_kind.builtin, inline_cmd_node.nodes[1:]))
        else:
            assert(isinstance(cmd_kind, CommandKinds.ExternalProgram))
            unknown_args = UnknownArray(0, 0)
            args = nodesToArgs(cmd_ctx, inline_cmd_node.nodes, unknown_args, ExpandNodeErrorContext())
            if isinstance(args, Error):
                return args
            if len(args) == 0 and unknown_args.empty():
                return CommandWithNoArgumentsError()
            pipe_stdout = ((i+1) < len(inline_cmd_nodes)) or pipe_last_command
            inline_cmds.append(PipeCommandExternalProgram(inline_cmd_ctx, args, unknown_args, pipe_stdout))

    if cmd_ctx.script.verification_mode:
        inline_cmds[0].cmd_ctx.capture.stdin = None
        i = 0
        while True:
            on_last = ((i + 1) == len(inline_cmd_nodes))
            if on_last:
                inline_cmds[i].cmd_ctx.capture.stdout = cmd_ctx.capture.stdout
            else:
                inline_cmds[i].cmd_ctx.capture.stdout = VerifyWriter(StringBuilder(), for_pipe=True)

            result = inline_cmds[i].run()
            stdout = None
            if not on_last:
                writer = inline_cmds[i].cmd_ctx.capture.stdout
                assert(isinstance(writer, VerifyWriter))
                if writer.builder:
                    stdout = writer.builder.output

            if isinstance(result, Error):
                return result
            if isinstance(result, ExitCode):
                if result.value != 0:
                    return NonZeroExitCodeError(result.value, stdout, None)
            elif isinstance(result, UnknownExitCode):
                pass
            else:
                raise Exception("TODO: unhandled result type from pipe command {}".format(type(result).__name__))

            if on_last:
                # we don't combine with the context output because stdout was set to the parent
                # context and stderr is currently never captured, so in both cases they should be null
                #return combineRunResultWithOutputs(inline_cmds[i].cmd_ctx, result)
                assert(inline_cmds[i].cmd_ctx.capture.stdout == cmd_ctx.capture.stdout)
                assert(inline_cmds[i].cmd_ctx.capture.stderr == cmd_ctx.capture.stderr)
                assert(isinstance(result, Error) or
                       isinstance(result, ExitCode) or
                       isinstance(result, String) or
                       isinstance(result, Bool) or
                       isinstance(result, UnknownExitCode) or
                       isinstance(result, UnknownString) or
                       isinstance(result, UnknownBool))
                if inline_cmds[i].cmd_ctx.capture.exitcode:
                    if isinstance(result, ExitCode):
                        return BOOL_TRUE if (result.value == 0) else BOOL_FALSE
                    assert(isinstance(result, UnknownExitCode))
                    return UNKNOWN_BOOL
                return result

            i += 1
            if stdout:
                inline_cmds[i].cmd_ctx.capture.stdin = StringReader(stdout)
            else:
                inline_cmds[i].cmd_ctx.capture.stdin = None

    for inline_cmd in inline_cmds:
        inline_cmd.prepare()

    last_stdout = inline_cmds[1].getPipeWriter()
    inline_cmds[0].cmd_ctx.capture.stdout = last_stdout
    i = 1
    while True:
        inline_cmds[i].cmd_ctx.capture.stdin = last_stdout.getPipeReader()
        if (i+1) == len(inline_cmds):
            break
        last_stdout = inline_cmds[i+1].getPipeWriter()
        inline_cmds[i].cmd_ctx.capture.stdout = last_stdout
        i += 1

    inline_cmds[ 0].cmd_ctx.capture.stdin  = cmd_ctx.capture.stdin
    inline_cmds[-1].cmd_ctx.capture.stdout = cmd_ctx.capture.stdout

    # NOTE: currently I'm using threads to execute each command in the pipe, but
    #       the final implementation will not be using threads.  It will probably
    #       use some form of multiplexed IO.
    threads: List[CommandThread] = []
    for i, inline_cmd_node in enumerate(inline_cmd_nodes):
        #print("DEBUG: start thread {}".format(i))
        thread = CommandThread(inline_cmds[i])
        thread.start()
        threads.append(thread)

    inline_cmd_results = []
    for i, inline_cmd_node in enumerate(inline_cmd_nodes):
        #print("DEBUG: joining thread {}...".format(i))
        threads[i].join()
        if i < len(inline_cmd_nodes) - 1:
            inline_cmd_results.append(threads[i].return_value)
        else:
            # assert that our context has no output that we need to return
            assert(inline_cmds[i].cmd_ctx.capture.stdout == cmd_ctx.capture.stdout)
            assert(inline_cmds[i].cmd_ctx.capture.stderr == cmd_ctx.capture.stderr)
            last_result = threads[i].return_value

    for inline_cmd_result in inline_cmd_results:
        if isinstance(inline_cmd_result, Error):
            return inline_cmd_result
        if isinstance(inline_cmd_result, ExitCode):
            if inline_cmd_result.value != 0:
                return inline_cmd_result
        else:
            raise Exception("unhandled pipe command result {}".format(type(inline_cmd_result).__name__))

    if isinstance(last_result, Error):
        return last_result
    if inline_cmds[-1].cmd_ctx.capture.exitcode:
        assert(isinstance(last_result, ExitCode))
        return BOOL_TRUE if (last_result.value == 0) else BOOL_FALSE
    if (isinstance(last_result, ExitCode) or
        isinstance(last_result, String) or
        isinstance(last_result, Bool)):
        return last_result
    raise Exception("unhandled result type {} for last pipe command".format(type(last_result).__name__))

RunCommandsResult = Union[Error,String,Bool,ExitCode,CommandResult,UnknownString,UnknownBool,UnknownExitCode,UnknownCommandResult]

def runCommandNodes(cmd_ctx: CommandContext, nodes: List[parse.Node]) -> RunCommandsResult:
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

    cmd_kind = getCommandKind(cmd_ctx, nodes)
    if isinstance(cmd_kind, Error):
        return cmd_kind
    if isinstance(cmd_kind, CommandKinds.Builtin):
        return runBuiltin(cmd_ctx, cmd_kind.builtin, nodes[1:])
    if isinstance(cmd_kind, CommandKinds.BinaryExp):
        result = runBinaryExpression(cmd_ctx, nodes, cmd_kind.kind)
        if (isinstance(result, Error) or
            isinstance(result, Bool) or
            isinstance(result, ExitCode) or
            isinstance(result, CommandResult) or
            (cmd_ctx.script.verification_mode and (
                isinstance(result, UnknownBool) or
                isinstance(result, UnknownExitCode) or
                isinstance(result, UnknownCommandResult)))):
            return result
        raise Exception("unhandled return type from runBinaryExpression {}".format(type(result).__name__))
    if isinstance(cmd_kind, CommandKinds.Bool):
        return SemanticError("unhandled Bool")
    assert(isinstance(cmd_kind, CommandKinds.ExternalProgram))
    unknown_args = UnknownArray(0, 0)
    args = nodesToArgs(cmd_ctx, nodes, unknown_args, ExpandNodeErrorContext())
    if isinstance(args, Error):
        return args
    if cmd_ctx.script.verification_mode:
        return UnknownExitCode()
    assert(isinstance(cmd_ctx.capture.stdout, RuntimeWriter))
    assert(isinstance(cmd_ctx.capture.stderr, RuntimeWriter))
    return runExternalProgram(cmd_ctx.capture.stdout, cmd_ctx.capture.stderr, args)

def nodesToArgs(cmd_ctx: CommandContext, nodes: List[parse.Node], unknown_args: UnknownArray, error_ctx: ExpandNodeErrorContext) -> Union[Error,List[bytes]]:
    args: List[bytes] = []
    for node in nodes:
        obj = expandNode(cmd_ctx, node, error_ctx)
        if isinstance(obj, Error):
            return obj
        error = objectToArgs(obj, args, unknown_args)
        if error:
            return error
    return args

def objectToArgs(obj: StitchObject, args: List[bytes], unknown_args: UnknownArray) -> Optional[Error]:
    assert(not isinstance(obj, Error))
    if isinstance(obj, String):
        args.append(obj.value)
        return None
    if isinstance(obj, Array):
        args.extend(obj.elements)
        return None
    if isinstance(obj, UnknownString):
        unknown_args.addScalar(1)
        return None
    if isinstance(obj, UnknownArray):
        unknown_args.addScalar(obj.min_count)
        if obj.max_count is None:
            unknown_args.max_count = None
        else:
            assert(obj.max_count >= obj.min_count)
            obj.max_count += obj.max_count - obj.min_count
        return None
    if isinstance(obj, Bool) or isinstance(obj, UnknownBool):
        return CannotCoerceToStringError("Bool")
    if isinstance(obj, BinaryEvaluator):
        return SemanticError("unexpected '{}'".format(obj))
    if isinstance(obj, Builtin):
        return CannotCoerceToStringError("Builtin '{}'".format(obj))

    return SemanticError("TODO: implement objectToArgs for type {} ({})".format(type(obj), obj))

def resolveExternalProgram(prog: bytes) -> Union[Error,bytes]:
    if os.name == "nt":
        if b"/" in prog or b"\\" in prog:
            return prog
    else:
        if b"/" in prog:
            return prog
    prog_filename = which(prog)
    if not prog_filename:
        return MissingProgramError(prog)
    return prog_filename

def runExternalProgram(stdout: RuntimeWriter, stderr: RuntimeWriter, args: List[bytes]) -> Union[Error,ExitCode]:
    if len(args) == 0:
        return CommandWithNoArgumentsError()

    prog = resolveExternalProgram(args[0])
    if isinstance(prog, Error):
        return prog
    args[0] = prog

    stdout_option = None if isinstance(stdout, ConsoleWriter) else subprocess.PIPE
    stderr_option = None if isinstance(stderr, ConsoleWriter) else subprocess.PIPE
    result = subprocess.run(args, stdout=stdout_option, stderr=stderr_option)
    if not isinstance(stdout, ConsoleWriter):
        stdout.handle(result.stdout)
    if not isinstance(stderr, ConsoleWriter):
        stderr.handle(result.stderr)
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

def stdoutOnlyHandler(stdout: bytes, multiline: bool) -> Union[Error,String]:
    assert(isinstance(stdout, bytes))
    if multiline:
        return String(stdout)
    lines = stdout.splitlines()
    if len(lines) == 0:
        return String(b"")
    elif len(lines) == 1:
        return String(stripNewline(lines[0]))
    return UnexpectedMultilineError(stdout)

def getCaptureWriterOutput(writer: Writer) -> Union[bytes,UnknownString]:
    if isinstance(writer, StringBuilder):
        assert(type(writer.output) == bytes)
        return writer.output
    assert isinstance(writer, VerifyWriter)
    if writer.builder:
        assert(type(writer.builder.output) == bytes)
        return writer.builder.output
    return UNKNOWN_STRING

def isCaptured(s: Optional[Union[bytes,UnknownString]]):
    return s != None

CombineResult = Union[Error,String,Bool,CommandResult,UnknownString,UnknownBool,UnknownCommandResult]

def combineRunResultWithOutputs(cmd_ctx: CommandContext, result: RunCommandsResult) -> CombineResult:
    if isinstance(result, Error):
        return result

    stdout: Optional[Union[bytes,UnknownString]] = None
    stderr: Optional[Union[bytes,UnknownString]] = None
    if cmd_ctx.parent:
        if cmd_ctx.capture.stdout != cmd_ctx.parent.capture.stdout:
            stdout = getCaptureWriterOutput(cmd_ctx.capture.stdout)
        if cmd_ctx.capture.stderr != cmd_ctx.parent.capture.stderr:
            stderr = getCaptureWriterOutput(cmd_ctx.capture.stderr)

    if isinstance(result, ExitCode) or isinstance(result, UnknownExitCode):
        capture_ec = cmd_ctx.capture.exitcode
        if (not capture_ec) and isinstance(result, ExitCode) and result.value != 0:
            return NonZeroExitCodeError(result.value,
                                        None if isinstance(stdout, UnknownString) else stdout,
                                        None if isinstance(stderr, UnknownString) else stderr)
        if capture_ec and (not isCaptured(stdout)) and (not isCaptured(stderr)):
            if isinstance(result, UnknownExitCode):
                return UNKNOWN_BOOL
            return BOOL_TRUE if (result.value == 0) else BOOL_FALSE
        if (not capture_ec) and isCaptured(stdout) and (not isCaptured(stderr)):
            if isinstance(result, ExitCode):
                if isinstance(stdout, UnknownString):
                    return UNKNOWN_STRING
                assert(isinstance(stdout, bytes))
                return stdoutOnlyHandler(stdout, result.multiline)
            assert(isinstance(result, UnknownExitCode))
            # TODO: should we be verifying this?
            #assert(isinstance(stdout, UnknownString))
            return UNKNOWN_STRING
        if (not capture_ec) and (not isCaptured(stdout)) and isCaptured(stderr):
            if isinstance(result, UnknownExitCode):
                return UNKNOWN_STRING
            assert(isinstance(stderr, bytes))
            return String(stderr)
        if isinstance(result, UnknownExitCode):
            return UNKNOWN_COMMAND_RESULT

        return CommandResult(result.value, stdout, stderr)

    if (isinstance(result, Bool) or isinstance(result, Array)
        or isinstance(result, UnknownBool) or isinstance(result, UnknownArray)):

        if cmd_ctx.capture.exitcode or isCaptured(stdout) or isCaptured(stderr):
            values = ""
            if isinstance(result, Bool) or isinstance(result, UnknownBool):
                values = "a Bool"
            else:
                assert(isinstance(result, Array) or isinstance(result, UnknownArray))
                values = "an Array"
            if cmd_ctx.capture.exitcode:
                values += " and an ExitCode"
            if isCaptured(stdout):
                values += " and stdout"
            if isCaptured(stderr):
                values += " and stderr"
            # the user needs to limit it with something like @exitcode or @captureobj
            return SemanticError("this inline command captured multiple objects, {}".format(values))
        return result

    raise Exception("expected an ExitCode, Bool or Array but got {}".format(result.userTypeDescriptor()))


# returns an array of strings and builtin objects
def expandNode(cmd_ctx: CommandContext, node: parse.Node, error_ctx: ExpandNodeErrorContext) -> Union[Error,StitchObject,UnknownString]:
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

    if isinstance(node, parse.NodeBinaryOp):
        return SemanticError("unexpected '{}'".format(parse.binaryOpUserString(node.kind)))

    if isinstance(node, parse.NodeInlineCommand):
        inline_cmd_ctx = cmd_ctx.createChild()
        result = runCommandNodes(inline_cmd_ctx, node.nodes)
        return combineRunResultWithOutputs(inline_cmd_ctx, result)

    if isinstance(node, parse.NodeMultiple):
        unknown_args = UnknownArray(0, 0)
        args = nodesToArgs(cmd_ctx, node.nodes, unknown_args, ExpandNodeErrorContext(inside_multiple=True))
        if isinstance(args, Error):
            return args
        if not unknown_args.empty():
            return UNKNOWN_STRING
        return String(b"".join(args))

    raise Exception("codebug, unhandled node type {}".format(type(node)))

def disableCaptureModifiers(cmd_ctx: CommandContext, error_context_str: str) -> Optional[Error]:
    if cmd_ctx.capture.exitcode:
        return SemanticError("@exitcode is not compatible with {}".format(error_context_str))
    if cmd_ctx.parent is not None and cmd_ctx.capture.stderr is not cmd_ctx.parent.capture.stderr:
        return SemanticError("@stderr is not compatible with {}".format(error_context_str))
    if cmd_ctx.parent is not None and cmd_ctx.capture.stdout is not cmd_ctx.parent.capture.stdout:
        if isinstance(cmd_ctx.capture.stdout, StringBuilder):
            assert(len(cmd_ctx.capture.stdout.output) == 0)
        elif isinstance(cmd_ctx.capture.stdout, PipeWriter):
            # assert unverified code?
            return SemanticError("commands with {} cannot be piped".format(error_context_str))
        else:
            assert(isinstance(cmd_ctx.capture.stdout, VerifyWriter))
            if cmd_ctx.capture.stdout.for_pipe:
                return SemanticError("commands with {} cannot be piped".format(error_context_str))
            if cmd_ctx.capture.stdout.builder:
                assert(len(cmd_ctx.capture.stdout.builder.output) == 0)
        cmd_ctx.capture.stdout = cmd_ctx.parent.capture.stdout
    return None

def runBinaryExpression(cmd_ctx: CommandContext, nodes: List[parse.Node], op_kind: BinaryOpKind) -> ExpressionResult:
    if op_kind == BinaryOpKind.ASSIGN:
        return runAssign(cmd_ctx, nodes)
    if op_kind == BinaryOpKind.PIPE:
        return runPipe(cmd_ctx, nodes)

    evaluator = binary_evaluators[op_kind]
    if cmd_ctx.ambiguous_op:
        return SemanticError("got binary expression inside ambiguous operator '{}', wrap inside (..parenthesis..)".format(cmd_ctx.ambiguous_op))

    error = disableCaptureModifiers(cmd_ctx, "binary expressions")
    if error:
        return error

    first_obj = expandNode(cmd_ctx, nodes[0], ExpandNodeErrorContext())
    if isinstance(first_obj, Error):
        return first_obj

    expression_result = evaluator.initialValue(first_obj)
    if isinstance(expression_result, Error):
        return expression_result
    if isinstance(evaluator, ChainableBinaryEvaluator) and not cmd_ctx.script.verification_mode and evaluator.shortcircuit(expression_result):
        return expression_result

    index = 1
    while True:
        if index + 1 == len(nodes):
            return SemanticError("missing operand after '{}'".format(evaluator))

        operand_result = expandNode(cmd_ctx, nodes[index+1], ExpandNodeErrorContext())
        if isinstance(operand_result, Error):
            return operand_result
        expression_result = evaluator.apply(cmd_ctx.script.verification_mode, expression_result, operand_result)
        if isinstance(expression_result, Error):
            return expression_result
        if isinstance(evaluator, ChainableBinaryEvaluator) and not cmd_ctx.script.verification_mode and evaluator.shortcircuit(expression_result):
            return expression_result

        index += 2
        if index == len(nodes):
            return expression_result

        next_op = nodes[index]
        if not isinstance(next_op, parse.NodeBinaryOp):
            if isinstance(next_op, parse.NodeToken):
                return SemanticError("expected '{}' operator but got token '{}'; commands must be wrapped with (...)".format(evaluator, next_op.s.decode('ascii')))
            return SemanticError("TODO: good error message for node that was expected to be an operand: {}".format(next_op))
        if next_op.kind != op_kind:
            return SemanticError("'{}' and '{}' cannot be chained".format(evaluator, parse.binaryOpUserString(next_op.kind)))

def runScript(script_ctx: ScriptContext, stdout: Writer, stderr: Writer, stdin: Optional[Reader]) -> Union[Error,ExitCode]:
    pos = 0
    while pos < len(script_ctx.src):
        nodes, end = parse.parseCommand(script_ctx.src, pos, script_ctx.allstringliterals)
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
            parent=None,
            depth=0,
            capture=Capture(False, stdout, stderr, stdin),
            builtin_prefix_count=0,
            ambiguous_op=None
        ), nodes)
        if isinstance(result, Error):
            return result
        if isinstance(result, Bool):
            return SemanticError("unhandled Bool whose value is {}".format(result.value))
        if isinstance(result, ExitCode):
            if result.value != 0:
                return result
        else:
            assert(script_ctx.verification_mode)
            if isinstance(result, NoReturn):
                # TODO: mark the current block as NoReturn and assert errors if there
                #       are more commands in this block
                pass
            elif isinstance(result, UnknownBool):
                return SemanticError("unhandled Bool".format())
            else:
                assert(isinstance(result, UnknownExitCode))

    if len(script_ctx.blockStack) != 1:
        return SemanticError("need more '@end'")
    return ExitCode(0)

def normalizeFilename(filename):
    # TODO: implement this
    return filename

def runFile(global_ctx: GlobalContext, full_filename: Optional[bytes], src: bytes,
            stdout: Writer, stderr: Writer, stdin: Optional[Reader]) -> Union[Error,ExitCode]:
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
                stdout_verifier = VerifyWriter(None, for_pipe=False)
                stderr_verifier = VerifyWriter(None, for_pipe=False)
                script_ctx = ScriptContext(global_ctx, full_filename, src, verification_mode=True)
                result = runScript(script_ctx, stdout_verifier, stderr_verifier, None)
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
                    #sys.exit("something printed to stderr during verification mode???")
                eprint("stitch: DEBUG: verification done on '{}'".format(full_filename_str))

        script_ctx = ScriptContext(global_ctx, full_filename, src, verification_mode=False)
        return runScript(script_ctx, stdout, stderr, stdin)
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
    result = runFile(global_ctx, full_filename, src, CONSOLE_WRITER, CONSOLE_WRITER, CONSOLE_READER)
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
