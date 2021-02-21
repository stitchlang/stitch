#!/usr/bin/env python3
import sys
import os
import subprocess
import re

special_regex = re.compile("$()")

class Node:
    pass
class NodeComment(Node):
    def __init__(self):
        pass
    def __repr__(self):
        return "Comment"
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
    if c_ord == ord("$"):
        return NodeRawString("$"), i+1
    if c_ord == ord("("):
        return NodeRawString("("), i+1
    if c_ord == ord(")"):
        return NodeRawString(")"), i+1
    if c_ord == ord("#"):
        return NodeComment(), len(src)
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
            if isinstance(dollar_node, NodeComment):
                break
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
        if isspaceOrd(c_ord) or c_ord == ord(")"):
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
        if next_ord == ord(")") or (next_ord == '$' and (i+1 < len(src) and ord(src[i+1]) == '#')):
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
        print(output)
    return None

class Builtin:
    def note(args, script_vars, capture_stdout):
        return handleBuiltinOutput("", capture_stdout)
    def echo(args, script_vars, capture_stdout):
        return handleBuiltinOutput(" ".join(args), capture_stdout)
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
        return handleBuiltinOutput("", capture_stdout)
    def oneline(args, script_vars, capture_stdout):
        if len(args) == 0:
            sys.exit("Error: the $oneline builtin requires at least one argument")
        s = runCommandExpanded(args, script_vars, capture_stdout=True, log_cmd=False)
        lines = s.splitlines()
        if len(lines) == 0:
            return handleBuiltinOutput("", capture_stdout)
        if len(lines) == 1:
            return handleBuiltinOutput(stripNewline(lines[0]), capture_stdout)
        sys.exit("Error: program '{}' returned {} lines, but expected 1".format(str(args[0]), len(lines)))
    def captureexitcode(args, script_vars, capture_stdout):
        if not capture_stdout:
            sys.exit("Error: $captureexitcode must be used in in a command-substitution")
        if len(args) == 0:
            sys.exit("Error: the $captureexitcode builtin requires at least one argument")
        returncode, stdout = runCommandExpandedNoFail(args, script_vars, capture_stdout=False, log_cmd=False)
        assert(stdout == None)
        return str(returncode)

class Obj:
    pass
class ObjBuiltinFunc:
    def __init__(self, name):
        self.name = name
    def __repr__(self):
        return "${}".format(self.name)
class ObjString:
    def __init__(self, value):
        self.value = value

builtin_objects = {
    "_": ObjString(" "),
    "note": ObjBuiltinFunc("note"),
    "echo": ObjBuiltinFunc("echo"),
    "set": ObjBuiltinFunc("set"),
    "setarray": ObjBuiltinFunc("setarray"),
    "oneline": ObjBuiltinFunc("oneline"),
    "captureexitcode": ObjBuiltinFunc("captureexitcode"),
}

def which(name):
    for path in os.environ["PATH"].split(os.pathsep):
        filename = os.path.join(path, name)
        if os.path.isfile(filename) and os.access(filename, os.X_OK):
            return filename
    return None

# escape the argument in a way that it could be re-used in another script
def execNodeToScriptSource(arg):
    if type(arg) == ObjBuiltinFunc:
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
    return runCommandExpanded(expandNodes(ast_nodes, script_vars), script_vars, capture_stdout=capture_stdout, log_cmd=True)

def runCommandExpanded(exec_nodes, script_vars, capture_stdout, log_cmd):
    returncode, stdout = runCommandExpandedNoFail(exec_nodes, script_vars, capture_stdout=capture_stdout, log_cmd=log_cmd)
    if returncode != 0:
        sys.exit("Error: program '{}' exited with code {}".format(prog, result.returncode))
    return stdout

def runCommandExpandedNoFail(exec_nodes, script_vars, capture_stdout, log_cmd):
    # I suppose this could happen if the whole command is just an expanded array that expands to nothing
    if len(exec_nodes) == 0:
        return 0, ("" if capture_stdout else None)
    # NOTE: I may not want to log the expanded command
    if log_cmd:
        print("+ {}".format(" ".join([execNodeToScriptSource(n) for n in exec_nodes])))
    prog = exec_nodes[0]
    if type(prog) == str:
        # check args
        for node in exec_nodes[1:]:
            if type(node) != str:
                sys.exit("The builtin function '{}' cannot be passed to an external program!".format(node))

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
        return result.returncode, stdout
    elif type(prog) == ObjBuiltinFunc:
        return 0, getattr(Builtin, prog.name)(exec_nodes[1:], script_vars, capture_stdout)
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
    sys.exit("Error: can only concatenate strings but got a function '${}'".format(part))

# returns an array of strings and builtin-function objects
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
            elif type(obj) is ObjBuiltinFunc:
                exec_nodes.append(obj)
            else:
                sys.exit("not impl, expand object ${} of type {}".format(node.id, type(obj)))
        elif type(node) is NodeCommandSub:
            result = runCommandNodes(node.nodes, script_vars, capture_stdout=True)
            assert(type(result) == str)
            exec_nodes.append(result)
        elif type(node) is NodeMultiple:
            sub_exec_nodes = expandNodes(node.nodes, script_vars)
            exec_nodes.append("".join([concatPartAsString(n) for n in sub_exec_nodes]))
        else:
            raise Exception("codebug, unhandled node type {}".format(type(node)))
    return exec_nodes


def runLine(filename, script_vars, line):
    line = line.rstrip()
    nodes = parseTopLevelCommand(line)
    if len(nodes) == 0:
        return
    result = runCommandNodes(nodes, script_vars, capture_stdout=False)
    assert(result == None)

def runFile(filename):
    script_vars = {}
    with open(filename, "r") as file:
        # read first line, to handle shebang line
        firstline = file.readline()
        if firstline:
            if not firstline.startswith("#"):
                runLine(filename, script_vars, firstline)
            while True:
                line = file.readline()
                if not line:
                    break
                runLine(filename, script_vars, line)

def main():
    cmd_args = sys.argv[1:]
    if len(cmd_args) == 0:
        sys.exit("Usage: prototype.py FILE")
    if len(cmd_args) != 1:
        sys.exit("Error: too many command-line arguments")
    filename = cmd_args[0]
    runFile(filename)

main()
