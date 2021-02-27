#!/usr/bin/env python3
import sys
import os
import subprocess
import re

special_regex = re.compile("$()")

class Context:
    def __init__(self, scriptfile, callerworkdir, verification_mode):
        self.script_vars = {
            "scriptfile": String(scriptfile),
            # NOTE: callerworkdir will need to be forwarded to any sub-scripts
            #       maybe I can just use an environment variable
            "callerworkdir": String(callerworkdir),
        }
        self.verification_mode = verification_mode

class Node:
    pass
# TODO: rename this to NodeToken
class NodeRawString(Node):
    def __init__(self, s):
        self.s = s
    def __repr__(self):
        return "RawString({})".format(self.s)
class NodeVariable(Node):
    def __init__(self, id):
        self.id = id
    def __repr__(self):
        return "Variable({})".format(self.id)
class NodeCommandSub(Node):
    def __init__(self, nodes):
        self.nodes = nodes
    def __repr__(self):
        return "NodeCommandSub({})".format(", ".join([str(n) for n in self.nodes]))
class NodeMultiple(Node):
    def __init__(self, nodes):
        self.nodes = nodes
    def __repr__(self):
        return "Multiple({})".format(", ".join([str(n) for n in self.nodes]))

def combineNodes(existing_node, new_node):
    if not existing_node:
        return new_node
    if type(existing_node) is NodeMultiple:
        existing_node.nodes.append(new_node)
        return existing_node
    return NodeMultiple([existing_node, new_node])

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

def parseRawString(src, i):
    if i == len(src):
        sys.exit("Error: got '$@' with nothing after it")
    sentinel_char = src[i]
    sentinel_ord = ord(sentinel_char)
    i += 1
    start = i
    while True:
        if i == len(src):
            sys.exit("Error: got '$@{}' is missing the terminating '{}' character".format(sentinel_char, sentinel_char))
        if ord(src[i]) == sentinel_ord:
            return NodeRawString(src[start:i]), i+1
        i += 1

def parseDollarExpr(src, i):
    if i == len(src):
        # TODO: add test for this error
        sys.exit("Error: got a '$' with nothing after it")
    c_ord = ord(src[i])
    if c_ord == ord("#"):
        return NodeRawString("#"), i+1
    if c_ord == ord("$"):
        return NodeRawString("$"), i+1
    if c_ord == ord("("):
        return NodeRawString("("), i+1
    if c_ord == ord(")"):
        return NodeRawString(")"), i+1
    if c_ord == ord("@"):
        return parseRawString(src, i+1)
    if not isIdOrd(c_ord):
        sys.exit("Error: expected [a-zA-Z0-9_.$] after '$' but got '{}'".format(chr(c_ord)))
    id_start = i
    while True:
        i += 1
        if i == len(src):
            id_end = i
            break
        c_ord = ord(src[i])
        if not isIdOrd(c_ord):
            id_end = i
            if c_ord == ord("$"):
                i += 1
            break
    return NodeVariable(src[id_start:id_end]), i

def parseNode(src, i):
    assert(i < len(src))
    assert(ord(src[i]) != "#")
    assert(not isspaceOrd(ord(src[i])))
    node = None
    mark = i
    while i < len(src):
        c_ord = ord(src[i])
        if c_ord == ord("$"):
            if i > mark:
                node = combineNodes(node, NodeRawString(src[mark:i]))
            dollar_node, dollar_str_limit = parseDollarExpr(src, i+1)
            mark = dollar_str_limit
            i = dollar_str_limit
            node = combineNodes(node, dollar_node)
            continue
        if c_ord == ord("("):
            if i > mark:
                node = combineNodes(node, NodeRawString(src[mark:i]))
            cmd_start = i+1
            cmd_subst_nodes, cmd_subst_limit = parseCommand(src, cmd_start)
            if cmd_subst_limit == len(src) or ord(src[cmd_subst_limit]) != ord(")"):
                # TODO: add test for this error
                sys.exit("Error: '(' is missing the closing paren ')'")
            node = combineNodes(node, NodeCommandSub(cmd_subst_nodes))
            mark = cmd_subst_limit + 1
            i = cmd_subst_limit + 1
            continue
        if isspaceOrd(c_ord) or c_ord == ord("#") or c_ord == ord(")"):
            break
        i += 1
    if i > mark:
        node = combineNodes(node, NodeRawString(src[mark:i]))
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

def handleBuiltinOutput(output, capture_stdout):
    if capture_stdout:
        return output
    if len(output) > 0:
        # TODO: should echo even being outputing a newline?
        print(output, end="\n" if (output[-1] != "\n") else "")
    return None

class BuiltinMethods:
    def note(context, args, capture_stdout):
        return CommandResult(0, handleBuiltinOutput("", capture_stdout), None, False)
    def echo(context, args, capture_stdout):
        arg_strings = []
        for sub_node in args:
            if type(sub_node) == str:
                arg_strings.append(sub_node)
            elif type(sub_node) == CommandResult:
                arg_string = sub_node.toStringArg()
                if isinstance(arg_string, Error):
                    return arg_string
                arg_strings.append(arg_string)
            else:
                return SemanticError("$echo does not support objects of type {}".format(sub_node.userTypeDescriptor()))
        return CommandResult(0, handleBuiltinOutput(" ".join(arg_strings), capture_stdout), None, False)
    def set(context, args, capture_stdout):
        if len(args) == 0:
            sys.exit("Error: the $set builtin requires at least one argument")
        if len(args) > 2:
            sys.exit("Error: the $set builtin can only accept 2 arguments but got {}".format(len(args)))
        varname = args[0]
        if type(varname) != str:
            return SemanticError("$set requires a String but got {}".format(varname.userTypeDescriptor()))
        value_obj = args[1]
        if type(value_obj) == str:
            value_string = String(value_obj)
        elif type(value_obj) == CommandResult:
            cmd_string = value_obj.toStringArg()
            if isinstance(cmd_string, Error):
                return cmd_string
            value_string = String(cmd_string)
        else:
            return SemanticError("don't know how to set string variable from {} object".format(value.userTypeDescriptor()))

        #print("DEBUG: setting variable '{}' to '{}'".format(varname, value_string))
        context.script_vars[varname] = value_string
        # NOTE: not returning the value here? Why? because the $set builtin doesn't output it.  Also, if
        #       a script wants the value, they can now acces it through the variable that is being set.
        return CommandResult(0, handleBuiltinOutput("", capture_stdout), None, False)
    def settmp(context, args, capture_stdout):
        if len(args) < 3:
            sys.exit("Error: the $settmp builtin requires at least 3 arguments")
        varname = args[0]
        value = args[1]
        command = args[2:]
        save = context.script_vars.get(varname)
        #print("DEBUG: settmp '{}' to '{}'".format(varname, value))
        context.script_vars[varname] = String(value)
        try:
            return runCommandExpanded(context, command, capture_stdout=capture_stdout, log_cmd=False)
        finally:
            if save:
                #print("DEBUG: settmp reverting '{}' back to '{}'".format(varname, save))
                context.script_vars[varname] = save
            else:
                #print("DEBUG: settmp reverting '{}' back to None".format(varname))
                del context.script_vars[varname]
    def multiline(context, args, capture_stdout):
        if len(args) == 0:
            sys.exit("Error: the $multiline builtin requires at least one argument")
        if not capture_stdout:
            return SemanticError("the $multiline builtin is only supported inside a command-substitution")
        result = runCommandExpanded(context, args, capture_stdout=True, log_cmd=False)
        if isinstance(result, Error):
            return result
        assert(type(result) == CommandResult)
        result.multiline = True
        return result
    def assert_(context, args, capture_stdout):
        if len(args) != 1:
            return SemanticError("$assert requires 1 argument bug got {}".format(len(args)))
        result = args[0]
        if isinstance(result, Error):
            return result
        if type(result) != Bool:
            return SemanticError("$assert expects a Bool but got a {}".format(objUserTypeDescriptor(result)))
        if not result.value:
            return AssertError()
        return CommandResult(0, handleBuiltinOutput("", capture_stdout), None, False)
    def not_(context, args, capture_stdout):
        if len(args) != 1:
            return SemanticError("$not requires 1 argument bug got {}".format(len(args)))
        result = args[0]
        if isinstance(result, Error):
            return result
        if type(result) != Bool:
            return SemanticError("$not expects a Bool but got a {}".format(objUserTypeDescriptor(result)))
        return TEST_RESULT_FALSE if result.value else TEST_RESULT_TRUE
    def call(context, args, capture_stdout):
        if len(args) == 0:
            sys.exit("Error: the $call builtin requires at least one argument")
        program_file = args[0]
        if len(args) > 1:
            sys.exit("Error: $call with more than just a program not implemented")
        return runFile(context, program_file, capture_stdout)


class Obj:
    pass
class Bool(Obj):
    def __init__(self, value):
        self.value = value
    def userTypeDescriptor(self):
        return "Bool"
TEST_RESULT_FALSE = Bool(False)
TEST_RESULT_TRUE = Bool(True)

class Builtin:
    def __init__(self, name):
        self.name = name
    def __repr__(self):
        return "${}".format(self.name)
    def userTypeDescriptor(self):
        return "Builtin"
class String:
    def __init__(self, value):
        self.value = value
    def userTypeDescriptor(self):
        return "String"

class BinaryOperator(Obj):
    def __init__(self, name):
        self.name = name
        self.src_name = "$" + name
    def __repr__(self):
        return self.src_name
class ChainableBinaryOperator(BinaryOperator):
    def __init__(self, name):
        BinaryOperator.__init__(self, name)
class AndOperator(ChainableBinaryOperator):
    def __init__(self):
        ChainableBinaryOperator.__init__(self, "and")
    def initialValue(self, context, stdout_handler, operand):
        return operandToBool(context, stdout_handler, self, operand)
    def apply(self, context, stdout_handler, left: Bool, right):
        assert(context.verification_mode or left.value)
        right_result = operandToBool(context, stdout_handler, self, right)
        if isinstance(right_result, Error):
            return right_result
        return right_result
    def shortcircuit(self, result: Bool):
        return not result.value
class OrOperator(ChainableBinaryOperator):
    def __init__(self):
        ChainableBinaryOperator.__init__(self, "or")
    def initialValue(self, context, stdout_handler, operand):
        return operandToBool(context, stdout_handler, self, operand)
    def apply(self, context, stdout_handler, left: Bool, right):
        assert(context.verification_mode or not left.value)
        right_result = operandToBool(context, stdout_handler, self, right)
        if isinstance(right_result, Error):
            return right_result
        return right_result
    def shortcircuit(self, result):
        return result.value
class EqOperator(BinaryOperator):
    def __init__(self):
        BinaryOperator.__init__(self, "eq")
    def initialValue(self, context, stdout_handler, operand):
        if type(operand) == str:
            return operand
        return opInvalidTypeError(self, operand)
    def apply(self, context, stdout_handler, left, right):
        if type(right) == str:
            return TEST_RESULT_TRUE if (left == right) else TEST_RESULT_FALSE
        return opInvalidTypeError(self, right)
class CompareOperator(BinaryOperator):
    def __init__(self, name, func):
        BinaryOperator.__init__(self, name)
        self.func = func
    def initialValue(self, context, stdout_handler, operand):
        if type(operand) == str:
            # TODO: return semantic error if not a valid integer
            return int(operand)
        return opInvalidTypeError(self, operand)
    def apply(self, context, stdout_handler, left, right):
        assert(type(left) == int)
        if type(right) == str:
            # TODO: return semantic error if not a valid integer
            right_int = int(right)
            return TEST_RESULT_TRUE if self.func(left, right_int) else TEST_RESULT_FALSE
        return opInvalidTypeError(self, right)


def opInvalidTypeError(op, operand):
    return SemanticError("'{}' does not accept objects of type {}".format(op, objUserTypeDescriptor(operand)))

def operandToBool(context, stdout_handler, op, operand):
    if type(operand) == Bool:
        return operand
    if type(operand) == CommandResult:
        assert(operand.stderr == None)
        if operand.stdout != None:
            stdout_handler.handle(operand.stdout)
        return TEST_RESULT_TRUE if (operand.exitcode == 0) else TEST_RESULT_FALSE
    return opInvalidTypeError(op, operand)


def objUserTypeDescriptor(obj):
    if type(obj) == str:
        return "String"
    if isinstance(obj, Obj):
        return obj.userTypeDescriptor()
    raise Exception("objUserTypeDescriptor does not support '{}' yet".format(obj))

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
    "call": Builtin("call"),
    "assert": Builtin("assert_"),
    "false": TEST_RESULT_FALSE,
    "true": TEST_RESULT_TRUE,
    "not": Builtin("not_"),
    "or": OrOperator(),
    "and": AndOperator(),
    "eq": EqOperator(),
    "gt": CompareOperator("gt", CompareOp.gt),
    "lt": CompareOperator("lt", CompareOp.lt),
}

class CommandResult(Obj):
    def __init__(self, exitcode, stdout, stderr, multiline):
        self.exitcode = exitcode
        self.stdout = stdout
        self.stderr = stderr
        # indicates whethe multiline is allowed in toStringArg
        self.multiline = multiline
    def userTypeDescriptor(self):
        return "CommandResult"
    def toStringArg(self):
        # stderr not implemented
        assert(self.stderr == None)
        if self.exitcode != 0:
            return NonZeroExitCodeError(self)
        if self.multiline:
            return self.stdout
        lines = self.stdout.splitlines()
        if len(lines) == 0:
            return ""
        elif len(lines) == 1:
            return stripNewline(lines[0])
        raise Exception("here")
        return UnexpectedMultilineError(self)
        #sys.exit("Error: program '{}' returned {} lines, but command-subtitution requires only 1 line of output.  Prefix the command with '$multiline' to support multiple.".format(str(node.nodes[0]), len(lines)))
    def __repr__(self):
        return "CommandResult(exit={},stderr='{}',stdout='{}',multiline={})".format(
            self.exitcode, self.stderr, self.stdout, self.multiline)

class Error:
    def __init__(self, msg):
        self.msg = msg
class SemanticError(Error):
    def __init__(self, msg):
        Error.__init__(self, msg)
    def __repr__(self):
        return "SemanticError: {}".format(self.msg)
class AssertError(Error):
    def __init__(self):
        Error.__init__(self, "an assertion failed")
    def __repr__(self):
        return "AssertError"
class NonZeroExitCodeError(Error):
    def __init__(self, cmd_result: CommandResult):
        assert(cmd_result.exitcode != 0)
        Error.__init__(self, "command failed with exit code {}".format(cmd_result.exitcode))
        self.cmd_result = cmd_result
class MissingProgramError(Error):
    def __init__(self, prog):
        Error.__init__(self, "unable to find program '{}' in PATH".format(prog))
class UnexpectedMultilineError(Error):
    def __init__(self, cmd_result: CommandResult):
        Error.__init__(self, "received multiple lines from a command that was not prefixed with $multiline")
        self.cmd_result = cmd_result

def which(name):
    extensions = [""] if (os.name != "nt") else os.environ["PATHEXT"].split(";")
    for path in os.environ["PATH"].split(os.pathsep):
        for ext in extensions:
            filename = os.path.join(path, name + ext)
            if os.path.isfile(filename) and os.access(filename, os.X_OK):
                return filename
    return None

# escape the argument in a way that it could be re-used in another script
def execNodeToScriptSource(arg):
    if type(arg) == Builtin:
        return str(arg)
    assert(type(arg) == str)
    if (not special_regex.match(arg)) and (not " " in arg):
        return arg
    delimiter = None
    for delimiter_option in ('"', "'", '|'):
        if not delimiter_option in arg:
            delimiter = delimiter_option
            break
    if not delimiter:
        sys.exit("TODO: implement more delimiter options for this string '{}'".format(arg))
    return "$@{}{}{}".format(delimiter, arg, delimiter)

class StdoutCaptureHandler:
    def __init__(self):
        self.output = ""
    def combine(self, s):
        return self.output + s
    def handle(self, s):
        if len(s) > 0:
            self.output = s
            if s[-1] != "\n":
                self.output += "\n"

class StdoutPrintHandler:
    def __init__(self):
        pass
    def combine(self, s):
        return s
    def handle(self, s):
        print(s)

def runCommandNodes(context, ast_nodes, capture_stdout):
    stdout_handler = StdoutCaptureHandler() if capture_stdout else StdoutPrintHandler()
    nodes = expandNodes(context, stdout_handler, ast_nodes)
    if isinstance(nodes, Error):
        return nodes
    if type(nodes) == Bool:
        if capture_stdout:
            if len(stdout_handler.output) > 0:
                print(stdout_handler.output, end='')
        return nodes
    result = runCommandExpanded(context, nodes, capture_stdout=capture_stdout, log_cmd=True)
    if not capture_stdout:
        return result
    if isinstance(result, Error):
        return result
    if type(result) == Bool:
        if len(stdout_handler.output) > 0:
            print(stdout_handler.output, end='')
        return result
    assert(type(result) == CommandResult)
    return CommandResult(result.exitcode, stdout_handler.combine(result.stdout), result.stderr, result.multiline)

def runCommandExpanded(context, exec_nodes, capture_stdout, log_cmd):
    # I suppose this could happen if the whole command is just an expanded array that expands to nothing
    # NOTE: this should probably be a semantic error instead?
    if len(exec_nodes) == 0:
        return CommandResult(0, "" if capture_stdout else None, None, False)

    ## NOTE: I may not want to log the expanded command
    #if log_cmd:
    #    print("+ {}".format(" ".join([execNodeToScriptSource(n) for n in exec_nodes])))
    prog = exec_nodes[0]
    if type(prog) == str:
        return runProgram(prog, exec_nodes[1:], capture_stdout)
    elif type(prog) == Builtin:
        return getattr(BuiltinMethods, prog.name)(context, exec_nodes[1:], capture_stdout)
    elif type(prog) == CommandResult:
        # stderr not implemented
        assert(prog.stderr == None)
        prog_string = prog.toStringArg()
        if isinstance(prog_string, Error):
            return prog_string
        return runProgram(prog_string, exec_nodes[1:], capture_stdout)
    #elif type(prog) == Bool:
    #    return SemanticError("unhandled Bool")
    else:
        raise Exception("codebug: unhandled exec_node type {}".format(type(prog)))

def runProgram(prog: str, arg_nodes, capture_stdout):
    # check args
    for node in arg_nodes:
        if type(node) != str:
            return SemanticError("object of type '{}' cannot be passed to an external program".format(node.userTypeDescriptor()))

    if "/" in prog:
        args = [prog] + arg_nodes
    else:
        prog_filename = which(prog)
        if not prog_filename:
            return MissingProgramError(prog)
        args = [prog_filename] + arg_nodes
    # TODO: don't capture stderr
    result = subprocess.run(args, capture_output=capture_stdout)
    # TODO: what to do with stderr?
    stdout = None
    if capture_stdout:
        stdout = ""
        if result.stdout:
            stdout = result.stdout.decode("utf8")
    return CommandResult(result.returncode, stdout, None, False)

def lookupVar(context, name):
    obj = builtin_objects.get(name)
    if obj:
        return obj
    obj = context.script_vars.get(name)
    if obj:
        return obj
    return None

def concatPartAsString(part):
    if type(part) == str:
        return part
    if type(part) == CommandResult:
        return part.toStringArg()
    assert(type(part) is Builtin or type(part) is Bool)
    return SemanticError("can only concatenate strings but got '{}'".format(part.userTypeDescriptor()))


def tryAsBinaryOp(context, node):
    if type(node) is NodeVariable:
        obj = lookupVar(context, node.id)
        if isinstance(obj, BinaryOperator):
            return obj
    return None

def isArrayNode(context, node):
    # not fully implemented yet
    if type(node) is NodeVariable:
        return node.id == "expandemptyarray"
    return False

# returns an array of strings and builtin objects
def expandNonArrayNode(context, stdout_handler, node, node_index):
    assert(not isArrayNode(context, node))

    if type(node) is NodeRawString:
        return node.s

    if type(node) is NodeVariable:
        obj = lookupVar(context, node.id)
        if not obj:
            return SemanticError("'${}' is undefined".format(node.id))
        if type(obj) is String:
            return obj.value
        elif type(obj) is Builtin:
            return obj
        elif isinstance(obj, BinaryOperator):
            if node_index == 0:
                return SemanticError("missing operand before '{}'".format(obj))
            # this should have been caught by expandNodes
            assert(node_index != 1)
            return SemanticError("unexpected binary operator '{}'".format(obj))
        elif type(obj) is Bool:
            return obj
        else:
            sys.exit("not impl, expand object ${} of type {}".format(node.id, type(obj)))

    if type(node) is NodeCommandSub:
        result = runCommandNodes(context, node.nodes, capture_stdout=True)
        if isinstance(result, Error):
            return result
        elif type(result) == Bool:
            return result
        else:
            assert(type(result) == CommandResult)
            return result
    elif type(node) is NodeMultiple:
        sub_exec_nodes = expandNodes(context, stdout_handler, node.nodes)
        if isinstance(sub_exec_nodes, Error):
            return sub_exec_nodes
        strings = []
        for sub_node in sub_exec_nodes:
            s = concatPartAsString(sub_node)
            if isinstance(s, Error):
                return s
            strings.append(s)
        return "".join(strings)
    else:
        raise Exception("codebug, unhandled node type {}".format(type(node)))

def expandNodes(context, stdout_handler, nodes):
    if len(nodes) >= 2:
        op = tryAsBinaryOp(context, nodes[1])
        if op:
            return expandBinaryExpression(context, stdout_handler, nodes, op)

    exec_nodes = []
    for node_index, node in enumerate(nodes):
        if isArrayNode(context, node):
            # TODO: remove this special handling after I implement arrays
            #       for now I'm just including it for writing tests
            if node.id == "expandemptyarray":
                continue
        else:
            expanded = expandNonArrayNode(context, stdout_handler, node, node_index)
            if isinstance(expanded, Error):
                return expanded
            exec_nodes.append(expanded)
    return exec_nodes

def expandBinaryExpression(context, stdout_handler, nodes, op):
    assert(len(nodes) >= 2)
    if isArrayNode(context, nodes[0]):
        return SemanticError("array expansion cannot appear before '{}'".format(op))
    operand_result = expandNonArrayNode(context, stdout_handler, nodes[0], 0)
    if isinstance(operand_result, Error):
        return operand_result
    expression_result = op.initialValue(context, stdout_handler, operand_result)
    if isinstance(expression_result, Error):
        return expression_result
    if isinstance(op, ChainableBinaryOperator) and not context.verification_mode and op.shortcircuit(expression_result):
        return expression_result

    index = 1
    while True:
        if index + 1 == len(nodes):
            return SemanticError("missing operand after '{}'".format(op))

        if isArrayNode(context, nodes[index+1]):
            return SemanticError("array expansion cannot appear after '{}'".format(op))
        operand_result = expandNonArrayNode(context, stdout_handler, nodes[index+1], index+1)
        if type(operand_result) == SemanticError:
            return operand_result
        expression_result = op.apply(context, stdout_handler, expression_result, operand_result)
        if isinstance(expression_result, Error):
            return expression_result
        if isinstance(op, ChainableBinaryOperator) and not context.verification_mode and op.shortcircuit(expression_result):
            return expression_result

        index += 2
        if index == len(nodes):
            return expression_result

        next_op = nodes[index]
        if type(next_op) != NodeVariable:
            if type(next_op) == NodeRawString:
                return SemanticError("expected '{}' operator but got token '{}'; commands must be wrapped with (...)".format(op, next_op.s))
            return SemanticError("TODO: good error message for node that was expected to be an operand: {}".format(next_op))
        if next_op.id != op.name:
            return SemanticError("'{}' and '${}' cannot be chained".format(op, next_op.id))


def runLine(context, line, print_trace, capture_stdout):
    output = "" if capture_stdout else None

    nodes = parseTopLevelCommand(line)
    if len(nodes) == 0:
        return CommandResult(0, output, None, False)

    # Note: it seems like it might be better to just print the
    #       line in its source form rather than the expanded form
    #       definitely should have an option for this
    if print_trace:
        msg = "+ {}".format(line)
        if capture_stdout:
            output += msg + "\n"
        else:
            print(msg)

    result = runCommandNodes(context, nodes, capture_stdout=capture_stdout)
    if isinstance(result, Error):
        return result
    if type(result) == Bool:
        return result

    assert(type(result) == CommandResult)
    if capture_stdout:
        assert(type(result.stdout) == str)
        output += result.stdout
    else:
        assert(result.stdout == None)

    return CommandResult(result.exitcode, output, result.stderr, result.multiline)

def runFile(context, filename, capture_stdout):
    output = ""
    with open(filename, "r") as file:
        while True:
            line = file.readline()
            if not line:
                break
            line = line.rstrip()
            result = runLine(context, line, print_trace=True, capture_stdout=capture_stdout)
            if isinstance(result, Error):
                return result
            if type(result) == Bool:
                return SemanticError("unhandled Bool")

            assert(type(result) == CommandResult)
            if capture_stdout:
                assert(type(result.stdout) == str)
                if len(result.stdout) > 0:
                    output += result.stdout
                    if result.stdout[-1] != "\n":
                        output += "\n"
            else:
                assert(result.stdout == None)

            if result.exitcode != 0:
                return CommandResult(result.exitcode, output if capture_stdout else None, None, False)

    return CommandResult(0, output if capture_stdout else None, None, False)

def main():
    cmd_args = sys.argv[1:]
    if len(cmd_args) == 0:
        sys.exit("Usage: stitch FILE")
    if len(cmd_args) != 1:
        sys.exit("Error: too many command-line arguments")
    filename = cmd_args[0]
    full_filename = os.path.abspath(filename)

    # try to get rid of CWD state by going to a temporary readonly directory
    if os.name == "nt":
        sandbox_path = os.path.join(os.getenv("TEMP"), "stitch-sandbox")
    else:
        sandbox_path = "/tmp/stitch-sandbox"

    if not os.path.exists(sandbox_path):
        os.mkdir(sandbox_path)
    callerworkdir = os.getcwd()
    os.chdir(sandbox_path)

    #
    # TODO: implement this
    #
    verify_done = False
    #verify_context = Context(filename, callerworkdir, True)
    #result = runFile(verify_context, full_filename, capture_stdout=False)
    #if isinstance(result, Error):
    #    assert(type(result) == SemanticError)
    #    print("{}: SemanticError: {}".format(filename, result.msg))
    #    sys.exit(1)
    #    verify_done = True

    run_context = Context(full_filename, callerworkdir, False)
    result = runFile(run_context, full_filename, capture_stdout=False)
    if isinstance(result, Error):
        assert((type(result) != SemanticError) or (not verify_done))
        kind = "SemanticError" if (type(result) == SemanticError) else "Error"
        print("{}: {}: {}".format(filename, kind, result.msg))
        sys.exit(1)

    assert(type(result) == CommandResult)
    assert(result.stdout == None)
    assert(result.stderr == None)
    if result.exitcode:
        sys.exit("Error: the last progam exited with code {}".format(result.exitcode))

if __name__ == "__main__":
    main()
