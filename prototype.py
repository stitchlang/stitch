#!/usr/bin/env python3
import sys
import os
import subprocess
import re

special_regex = re.compile("$()")

class Node:
    pass
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

class Builtin:
    def note(args, script_vars, capture_stdout):
        return CommandResult(0, handleBuiltinOutput("", capture_stdout), None)
    def echo(args, script_vars, capture_stdout):
        return CommandResult(0, handleBuiltinOutput(" ".join(args), capture_stdout), None)
    def set(args, script_vars, capture_stdout):
        if len(args) == 0:
            sys.exit("Error: the $set builtin requires at least one argument")
        varname = args[0]
        if len(args) > 2:
            sys.exit("Error: the $set builtin can only accept 2 arguments but got {}".format(len(args)))
        value = args[1]
        #print("DEBUG: setting variable '{}' to '{}'".format(varname, value))
        script_vars[varname] = ObjString(value)
        # NOTE: not returning the value here? Why? because the $set builtin doesn't output it.  Also, if
        #       a script wants the value, they can now acces it through the variable that is being set.
        return CommandResult(0, handleBuiltinOutput("", capture_stdout), None)
    def settmp(args, script_vars, capture_stdout):
        if len(args) < 3:
            sys.exit("Error: the $settmp builtin requires at least 3 arguments")
        varname = args[0]
        value = args[1]
        command = args[2:]
        save = script_vars.get(varname)
        #print("DEBUG: settmp '{}' to '{}'".format(varname, value))
        script_vars[varname] = ObjString(value)
        try:
            return runCommandExpanded(command, script_vars, capture_stdout=capture_stdout, log_cmd=False)
        finally:
            if save:
                #print("DEBUG: settmp reverting '{}' back to '{}'".format(varname, save))
                script_vars[varname] = save
            else:
                #print("DEBUG: settmp reverting '{}' back to None".format(varname))
                del script_vars[varname]
    def multiline(args, script_vars, capture_stdout):
        if len(args) == 0:
            sys.exit("Error: the $multiline builtin requires at least one argument")
        if not capture_stdout:
            sys.exit("Error: the $multiline builtin is only supported inside a command-substitution")
        return MultilineResult(runCommandExpanded(args, script_vars, capture_stdout=True, log_cmd=False))
    def call(args, script_vars, capture_stdout):
        if len(args) == 0:
            sys.exit("Error: the $call builtin requires at least one argument")
        program_file = args[0]
        if len(args) > 1:
            sys.exit("Error: $call with more than just a program not implemented")
        return runFile(program_file, capture_stdout)

class Obj:
    pass
class ObjBuiltin:
    def __init__(self, name):
        self.name = name
    def __repr__(self):
        return "${}".format(self.name)
class ObjString:
    def __init__(self, value):
        self.value = value

class ObjBinaryTestOperator:
    def __init__(self, name):
        self.name = name

# builtin objects that do not change and are the same for all scripts
builtin_objects = {
    "note": ObjBuiltin("note"),
    "echo": ObjBuiltin("echo"),
    "set": ObjBuiltin("set"),
    "setarray": ObjBuiltin("setarray"),
    "settmp": ObjBuiltin("settmp"),
    "multiline": ObjBuiltin("multiline"),
    "call": ObjBuiltin("call"),
    "or": ObjBinaryTestOperator("or"),
    "and": ObjBinaryTestOperator("and"),
}

class MultilineResult:
    def __init__(self, value):
        self.value = value
class CommandResult:
    def __init__(self, exitcode, stdout, stderr):
        self.exitcode = exitcode
        self.stdout = stdout
        self.stderr = stderr
class SemanticError:
    def __init__(self, msg):
        self.msg = msg

def which(name):
    for path in os.environ["PATH"].split(os.pathsep):
        filename = os.path.join(path, name)
        if os.path.isfile(filename) and os.access(filename, os.X_OK):
            return filename
    return None

# escape the argument in a way that it could be re-used in another script
def execNodeToScriptSource(arg):
    if type(arg) == ObjBuiltin:
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

def runCommandNodes(ast_nodes, script_vars, capture_stdout):
    nodes = expandNodes(ast_nodes, script_vars)
    if type(nodes) == SemanticError:
        return nodes
    return runCommandExpanded(nodes, script_vars, capture_stdout=capture_stdout, log_cmd=True)

def runCommandExpanded(exec_nodes, script_vars, capture_stdout, log_cmd):
    # I suppose this could happen if the whole command is just an expanded array that expands to nothing
    if len(exec_nodes) == 0:
        return CommandResult(0, "" if capture_stdout else None, None)

    # handle binary operators
    # TODO: handle binary operators
    node_index = 0
    for node in exec_nodes:
        if type(node) == ObjBinaryTestOperator:
            if node_index == 0:
                return SemanticError("missing argument before binary operator '${}'".format(node.name))
            sys.exit("TODO: implement binary operators")
        node_index += 1

    ## NOTE: I may not want to log the expanded command
    #if log_cmd:
    #    print("+ {}".format(" ".join([execNodeToScriptSource(n) for n in exec_nodes])))
    prog = exec_nodes[0]
    if type(prog) == str:
        # check args
        for node in exec_nodes[1:]:
            if type(node) != str:
                sys.exit("The builtin '{}' cannot be passed to an external program!".format(node))

        if "/" in prog:
            args = exec_nodes
        else:
            prog_filename = which(prog)
            if not prog_filename:
                sys.exit("Error: cannot find program '{}'".format(prog))
            args = [prog_filename] + exec_nodes[1:]
        # TODO: don't capture stderr
        result = subprocess.run(args, capture_output=capture_stdout)
        # TODO: what to do with stderr?
        stdout = None
        if capture_stdout:
            stdout = ""
            if result.stdout:
                stdout = result.stdout.decode("utf8")
        return CommandResult(result.returncode, stdout, None)
    elif type(prog) == ObjBuiltin:
        return getattr(Builtin, prog.name)(exec_nodes[1:], script_vars, capture_stdout)
    else:
        sys.exit("type is {}".format(type(prog)))

def lookupVar(name, script_vars):
    obj = builtin_objects.get(name)
    if obj:
        return obj
    obj = script_vars.get(name)
    if obj:
        return obj
    return None

def concatPartAsString(part):
    if type(part) == str:
        return part
    assert(type(part) is ObjBuiltin)
    sys.exit("Error: can only concatenate strings but got a builtin '${}'".format(part))

# returns an array of strings and builtin objects
def expandNodes(nodes, script_vars):
    exec_nodes = []
    for node in nodes:
        if type(node) is NodeRawString:
            exec_nodes.append(node.s)
        elif type(node) is NodeVariable:
            obj = lookupVar(node.id, script_vars)
            if not obj:
                sys.exit("${} is undefined".format(node.id))
            if type(obj) is ObjString:
                exec_nodes.append(obj.value)
            elif type(obj) is ObjBuiltin:
                exec_nodes.append(obj)
            elif type(obj) is ObjBinaryTestOperator:
                exec_nodes.append(obj)
            else:
                sys.exit("not impl, expand object ${} of type {}".format(node.id, type(obj)))
        elif type(node) is NodeCommandSub:
            result = runCommandNodes(node.nodes, script_vars, capture_stdout=True)
            if type(result) == MultilineResult:
                exec_nodes.append(result.value)
            elif type(result) == SemanticError:
                return result
            else:
                assert(type(result) == CommandResult)
                if result.exitcode != 0:
                    sys.exit("a command-substitution got a command with a non-zero exit code, what to do?")
                lines = result.stdout.splitlines()
                if len(lines) == 0:
                    exec_nodes.append("")
                elif len(lines) == 1:
                    exec_nodes.append(stripNewline(lines[0]))
                else:
                    sys.exit("Error: program '{}' returned {} lines, but command-subtitution requires only 1 line of output.  Prefix the command with '$multiline' to support multiple.".format(str(node.nodes[0]), len(lines)))
        elif type(node) is NodeMultiple:
            sub_exec_nodes = expandNodes(node.nodes, script_vars)
            if type(sub_exec_nodes) == SemanticError:
                return sub_exec_nodes
            exec_nodes.append("".join([concatPartAsString(n) for n in sub_exec_nodes]))
        else:
            raise Exception("codebug, unhandled node type {}".format(type(node)))
    return exec_nodes


def runLine(line, script_vars, print_trace, capture_stdout):
    output = "" if capture_stdout else None

    nodes = parseTopLevelCommand(line)
    if len(nodes) == 0:
        return CommandResult(0, output, None)

    # Note: it seems like it might be better to just print the
    #       line in its source form rather than the expanded form
    #       definitely should have an option for this
    if print_trace:
        msg = "+ {}".format(line)
        if capture_stdout:
            output += msg + "\n"
        else:
            print(msg)

    result = runCommandNodes(nodes, script_vars, capture_stdout=capture_stdout)
    if type(result) == SemanticError:
        return result

    assert(type(result) == CommandResult)
    if capture_stdout:
        assert(type(result.stdout) == str)
        output += result.stdout
    else:
        assert(result.stdout == None)

    return CommandResult(result.exitcode, output, result.stderr)

def createScriptVars(scriptfile):
    return {
        "scriptfile": ObjString(scriptfile),
    }

def runFile(filename, capture_stdout):
    script_vars = createScriptVars(filename)
    output = ""
    with open(filename, "r") as file:
        while True:
            line = file.readline()
            if not line:
                break
            line = line.rstrip()
            result = runLine(line, script_vars, print_trace=True, capture_stdout=capture_stdout)
            if type(result) == SemanticError:
                return result

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
                return CommandResult(result.exitcode, output if capture_stdout else None, None)

    return CommandResult(0, output if capture_stdout else None, None)

def main():
    cmd_args = sys.argv[1:]
    if len(cmd_args) == 0:
        sys.exit("Usage: prototype FILE")
    if len(cmd_args) != 1:
        sys.exit("Error: too many command-line arguments")
    filename = cmd_args[0]
    full_filename = os.path.abspath(filename)

    # try to get rid of CWD state by going to a temporary readonly directory
    if not os.path.exists("/tmp/script-sandbox"):
        os.mkdir("/tmp/script-sandbox")
        os.chdir("/tmp/script-sandbox")

    result = runFile(full_filename, capture_stdout=False)
    if type(result) == SemanticError:
        print("{}: SemanticError: {}".format(filename, result.msg))
        sys.exit(1)

    assert(type(result) == CommandResult)
    assert(result.stdout == None)
    assert(result.stderr == None)
    if result.exitcode:
        sys.exit("Error: the last progam exited with code {}".format(result.exitcode))

if __name__ == "__main__":
    main()
