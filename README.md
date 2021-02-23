# stitch

A scripting language that creates programs by "stitching" other programs together.  An alternative to other languages like BASH with a focus on making it easy to write "correct programs".

# Preview

```sh
# this is a comment
$echo Hello, World

# variables
$set name Fred
$echo Hello $name

# command substitution
$set arch (uname -m)

# arrays
$setarray names args Fred Lisa Joey
$echo Hello $expand.names
```

stitch scripts primarily consist of commands containing one or more arguments.  The first argument is inspected to determine how the command is invoked; it can be:

* a builtin program (like `$echo`)
* a program filename (if it contains any slash `/` characters)
* a program name (a file in one of the `PATH` directories)

# Argument Separation Before Expansion

One way to make it easier to write "correct programs" is if something looks like a duck, it should be a duck.  One common pitfall of other scripting languages is that program arguments are not always separated as they appear to be in the script.  For example, the command `cat $myfile` would call `cat` with any number of arguments depending on the contents of `$myfile` even though it appears like it is a single argument.  In stitch, this would always be a single argument.  This is accomplished by separating arguments before variable expansion.  In the case where `$myfile` was actually meant to be any number of arguments, array expansion can be used `cat $expand.myfiles`.

# Special Characters

stitch has very few special characters that cause it to deviate from the normal mode of specifying programs/arguments:

Char | Description
-----|-------------
`#`  | a single-line comment, escape with `$#`
`$`  | starts an expression, escape with `$$`
`(`  | start a command substitution, escape with `$(`
`)`  | ends a command substitution, escape with `$)`
` `  | separates command arguments, using strings to include them instead

# Arguments with Spaces

```sh
$echolines arg1 arg2 arg3
# prints:
#   arg1
#   arg2
#   arg3

$echolines arg 1 arg 2 arg 3
# prints:
#   arg
#   1
#   arg
#   2
#   arg
#   3

$echolines $@"arg 1" $@"arg 2" $@"arg 3"
# prints:
#   arg 1
#   arg 2
#   arg 3

$echolines arg$sp$1 arg$sp$2 arg$sp$3
# prints:
#   arg 1
#   arg 2
#   arg 3

# NOTE: I can use command substituion to include spaces in a string like this
$echolines ($echo arg 1) ($echo arg 2) ($echo arg 3)
# prints:
#   arg 1
#   arg 2
#   arg 3
```

# Arguments with special characters

```sh
# awk is a good example to demonstrate because it also makes use of $
awk $@"{print $1 $2}"
```

# When not to use stitch

stitch's primary purpose is to call other programs, when this is not the primary purpose of a script, another language like Python is better suited.

# Variables

```sh
#
# $set [SCOPE.]VARNAME VALUE
#
$set msg $@"Hello, my name is Fred"
$echo $msg
# prints "Hello, my name is Fred"

$echo $g.msg
# still prints "Hello, my name is Fred", "g" is a special scope through which user variables can be accessed.  This should be used for variables that may conflict with predefined variables (i.e. $g.echo instead of $echo). (Note: `$g.g` is not the same as `$g`, it would be a user script variable named `g`).

$set cpu_count (nproc)

$echo You have $cpu_count cpus
$echo Expand cpu count with a suffix is $cpu_count$cpus
```

Builtin variables

Name   | C Equivalent
-------|---------
sp     | The string " "
lf     | The string "\n"
cr     | The string "\r"
crlf   | The string "\r\n"
nl     | Either "\n" or "\r\n" depending on platform
tab    | The string "\t"
g      | The script's global scope.
env    | Environment variable scope.
expand | Scope to expand arrays through.

> NOTE: maybe support $XX$ to insert arbitrary hex characters?  So a space could be $20$?

# Arrays

Arrays are important because they provide a way to represent one or more strings without requiring them to be delimited.  Not having an array type and requiring a delimiter instead is the source of many pitfalls with other scripting languages.

```sh
#
# $setarray [SCOPE.]VARNAME splitlines VALUE
# $setarray [SCOPE.]VARNAME args ARGS...
#
# TODO: splitwhitespace?
#
# Example:
$setarray mounts splitlines (cat /proc/mounts)

# now the "mounts" variable is an array of all the mounts

# How to use the array in a command?
$echolines $expand.mounts

# Not using $expand.VAR will cause an error
$echolines $mounts
# Error: mounts is an array, you must expand it with $expand.mounts
```

Note that arrays cannot be used within another string.  They must be expanded on their own.  The reason for using the `expand` scope to expand arrays, is so that it's immediately apparent that an array is being expanded into 0 or more arguments.

# Environment variables:

Environment variables are accessed through the `env` scope.

```sh
$env.VAR

$set env.VAR VALUE
```

# Command Substitution

Command Subtitution is expected to be a very common construct in this language.  Note that Command Substitution runs a command and returns stdout of that command as a string.  Unlike other scripting languages, the command is executed within the current process environment, it is not executed in a subprocess so it has very low overhead.

```sh
# run command and return one line of output as a string with no trailing newline
(PROG ARGS...)

# run the command and return any number of lines of output
($multiline PROG ARGS...)
```

In general Command Subtitution is commonly used to return strings that don't contain newlines.  Because of this, by default Command Substitution only allows up to one line of output from stdout of the underlying command.  It also strips the trailing newline from the output before returning the string.

This default behavior is overriden by prefixing the comand with `$multiline`.  This will return stdout of the underlying process unmodified.

```sh
#echo you're arch is (uname -m)

$set myfile_content ($multiline cat myfile)
$set lsfile (which ls)

# Example
make -j(nproc)
```

# Examples

### Hello World

```sh
$echo Hello World
```

### Read input and print it

```sh
$set input (read)
$echo You entered: $input
```

### Random

```sh
$set pwd (pwd)
$set arch (uname -m)
$set target $arch$-linux-musl
$set prefix $env.HOME/$target
$set jobs -j(nproc)

wget example.come/$target$.tar.xz
tar xf $target$.tar.xz

$set env.PATH $pwd/$target/bin:$env.PATH
```

# Builtin Programs

Because this scripting language uses named builtin's, this enables the language to provide many features without the need to add new syntax.  Among these features include cross-platform builtin programs.  When considering whether the language should provide a builtin, these questions can be analyzed:

1. How simple is the feature?
2. How common/generally useful is the feature?

If a feature is "simple" and provides some sort of benefit, then there's not much downside to including it in the language.  A few lines of code is all that's needed to include it in the language.  The only downside I see is feature overlap/multiple ways to do the same thing which should be avoided if it can.

If a feature is not so simple, then it still may be a candidate for inclusion if it is common and/or generally useful enough.

#### $multiline PROG ARGS...

Runs the given program output wrapped in an internal "MultilineResult" object that allows strings with mulitple lines to be returned from a command-substitution.

#### $firstline/$lastline

Not sure if these should be added yet. I've included them to be considered.

#### $findprog NAME

Assuming programs are located the same way as BASH and/or execve, I should expose this logic through a builtin.

# WYSIWYG Strings

WYSIWYG strings make it easier to write correct code because they are easy for humans to verify and easy to copy between applications. To support them, we need a way to disable our special characters `#`, `$`, `(`, `)` and ` `.  A WYSIWYG string is started with the sequence `$@` followed by a delimiter character.  The string continues until it sees the delimiter character again.  Here are some examples:

```sh
$echo $@"I can use #, $, ( and ) in here but not a double-quote"
# prints:
#   I can use #, $, ( and ) in here but not a double-quote

$echo $@'I can use #, $, (, ) and " but not single-quote'
# prints:
#   I can use #, $, (, ) and " but not single-quote

$echo $@|I can use #, $, (, ), " and ' but not a pipe character in here|
# prints:
#   I can use #, $, (, ), " and ' but not a pipe character in here
```

The scripting language should also probably include a way to create multiline strings.  I'll decide on this later.

It might also be good to include some shorthand variations like this:
```sh
$echo $" example 1 "
$echo $' example 1 '
$echo $| example 1 |

# probably don't do $( ... ) because that could easily get confused with command-substitution
# maybe just $"..." and $'...'
```

I could also support `"..."`.  This would make the double-quote `"` character a special reserved character. Would this make it easier to write correct programs? Keeping the number of reserved characters low makes it simpler to reason about what source is doing.  The question is whether that benefit outweighs needing to type `$"..."` instead of `"..."`.

Note that the same reasoning that applies to WYSIWYG strings would also apply to HEREDOC strings. A special dollar keyword could start a heredoc, tell if it is raw or processed, then specify the sentinel delimiter.

# Binary Expressions

Binary expressions are distinct from Commands. The operands between binary operators are limited to a "single node".

```sh
#
# Binary Expression Syntax:
#
Node BinaryOperator Node


# OK
$a $and $b

# Syntax Error
grep foo bar $and $b

# OK
(grep foo bar) $and $b
```

The `(grep foo bar)` operand in the example above looks like normal "Command Substitution" but has some differences.  Instead of returning stdout of the underlying command, it returns a TestResult object based on the exit code.  This change in behavior also imples that:

1. a non-zero exit code from the command does not cause the script to exit
2. since the return channel has been taken by the TestResult object, stdout is no longer captured and is printed like a normal command

Currently there are only 2 binary operators: `$and` and `$or`.  Here are some more candidates:

```sh
Node $equals Node
Node $lessorequal Node
Node $greaterorequal Node
Node $less Node
Node $greater Node

# or maybe
Node $= Node
Node $== Node
Node $<= Node
Node $>= Node
Node $< Node
Node $> Node
```

### Short Circuting

If the final result of a binary expression has been determined before it has been fully evaluated, the language does not expand the rest of the expression.  Because of this, commands must be checked for binary operators before expanding them.  Since this requires special handling of node expansion within the binary expression, this provides the information we need to enable modified command-substitution.

### Binary Expression TODO/Questions

* Top Level Handling?

How should binary expressions be handled at the top-level?  For now I've just made them an error "uhandled TestResult".

* Unary Expressions?

I don't think the language needs an special handling for unary expressions.  I believe these can be handled by builtins, like:

```sh
$exists PATH
$isdir PATH
$isfile PATH
```


# Control Flow

### First Idea for if/while:

```sh
#
# if/elif/else/endif
#
$if command
    command
    command
    ...
$elif command
    command
    command
    ...
$else
    command
    command
    ...
$end

#
# while/continue/break
#
$while command
    command
    command
    ...

    $if command
        $continue
    $if command
        $break

$end
```

```sh
# returns a CommandResult "top-level handler" which causes a fatal error on non-zero exit code
grep needle file

# $if will both zero and non-zero exit codes from a CommandResult and use it to decide on the brancht to execute
$if grep needle file
    $echo found needle!
$else
    $echo did not find needle
$end

# unary operator example

# applying a "test operator" to a CommandResult returns a TestResult
grep needle file
$not grep needle file

# both of the commands above would cause an "unhandle TestResult" if they appeared at the top-level
```

So any command that is given to a "test operator" must bubble up to a control flow builtin like `$if`, `$while`.

```sh
$if $not grep needle file $and $not grep needle2 file
    $echo both needles are not found
$else
    $echo at least one needle is present
$end
```

So, unary operators are always higher precedence than binary operators.  What if we want to override that?

```sh
$if $not (grep needle file $and grep needle2 file)
    $echo at least one needle is missing
$else
    $echo both needls are present
$end
```

I think the example above just WORKS.  Command substitution takes any TestResult and propogates it up as a TestResult.  If a TestResult is attempted to be used as a string, it is an error, i.e.

```sh
ls ($not grep needle file)
#
# error: expected a string but got TestResult
#
```

What if we required parenthesis around binary operators?

```sh
# maybe this is an error?
$not grep needle file $and grep needle2 file

# need this
($not grep needle file) $and (grep needle2 file)
```

I think this makes things more clear and readable.  After a command is expanded, binary operators only support one object to their left and right.  If there are more then it's an error.  What about chaining binary operators?

```sh
# OK
foo $and bar $and baz

# This looks confusing
foo $or bar $and baz
```

I think the right thing to do here is to only allow chaining binary operators if they are the same operator.

# Grammar

What I've got so far
```
Script ::= Command*

Command ::= Argument* | BinaryExpression

Argument ::= (Char | '$' DollarExpression)* | BinaryExpression

DollarExpression ::= '(' Command ')' | [a-zA-Z_.]* '$'?

BinaryExpression ::= Operand Op Operand ( Op Operand )*

Operand ::= '(' Command ')' | Argument
```

> NOTE: "BinaryExpression" takes "Operand" instead of "Argument"  because these nodes are evaluated differently

# Idea: stderr/exitcode

How to handle return code and stdout/stderr?  I could have a CommandResult type of object.
```sh
#
# $captureouts PROG ARGS...
# $captureall     PROG ARGS...
#
# the $capture* builtins will run the program/args that follow, and instead of just returning stdout, it will return
# an object.  $capturestreams will capture stdout and stderr via the "out" and "err" fields.  $captureall will also capture
$ the exit code in the field `code`, note that this overrides the default behavior of exiting on a non-zero exit code.

$set some_program_result ($captureouts some-program)
$echo the stdout of some_program_result is: $some_program_result.out
$echo the stderr of some_program_result is: $some_program_result.err

$set another_program_result ($captureouts another-program)
$echo the stdout of another_program_result is: $another_program_result.out
$echo the stderr of another_program_result is: $another_program_result.err
$echo the exit code of another_program_result is: $another_program_result.code
```

# Idea: piping

UPDATE: I think piping *might* be a binary operator, so I need to update this section to reflect how it would work with binary operators

Piping is a pretty useful feature.  You could emulate piping by setting output to variables, however, piping directly from one process to another could remove extra copies of the output.

A running process has 2 objects, the stdout and stderr file stream.  Once the process has finished, it returns 3 objects, a stdout string, a stderr string and an exit code.

A script should be able to forward stdout and/or stderr from one process to the stdin of another.  Note that to forward stdout/stderr to a named file that can be read by another program, pipe files can be used.  This means the scripting language would need a way to redirect stdout and/or stderr to a named file as well.  Note that this could be done using a pipe operator that goes to a builtin command that forwards it to a file (means we don't need a redirect feature).

Syntax
```sh

foo $out bar
foo $err bar
foo $outerr bar

# How to pipe stdout and stderr to different places?

# the following won't work because $err could belong to the previous command
foo $out COMMAND... $err COMMAND...


# to redirect the output of "foo" to a file "myfile", use $in2file
foo $out $in2file myfile

# to forward out/err to different places, could do multiple lines like this
$set foo $stage foo args...
# the $stage COMMAND... builtin will create create a process that has not been started yet
# now $myprogram.out and $myprogram.err are open file handles
$attach $foo.out bar
$attach $foo.err baz
$run $foo
```

# The Current Working Directory

Having a current working directory as hidden state that affects all relative path names may be more trouble than it's worth.  One alterative is to use absolute path names.  However, some programs use the current working directory as an important input, in which case there needs to be a way to set it.  Here's a way we could set this and make it excplicit:

```sh
$cwd DIR PROG ARGS...

# example
$cwd $scriptdir git status

# here's the same example not using CWD, if the program supports it, then this is probably preferred
git -C $scriptdir status
```

# TODO:

* as a test, I should run a script, output all the commands that were run to another script, then run that script a second time and it should still work.  Assuming there is a `--dump-commands-to FILE` command-line option:

```sh
./script --dump-commands-to script2
./script2
```

Also compare that they produce the same output.

* Look at BASH "Gotchas" https://tldp.org/LDP/abs/html/gotchas.html.  See how many we avoided.

# Thoughts

### Script instead of Shell

This scripting language is meant to represent coherent programs that live inside files rather than individual commands typed by a user.  For this reason the term "script" seems more fitting than the term "shell".

### "always fails" is better than "sometimes works"

Having code that "sometimes works" is where bugs thrive.  It usually means the code will work in the test environment but then fail when it gets to the customer.  With this in mind, it's better to make features that either "fail all of the time" or "work all of the time".  If possible, avoid features that encourage code to "sometimes work".

### Why Command Substitution is `(...)` rather than `$(...)`

This syntax is a result of the following 3 observations:

1. I don't want `)` to be treated differently inside or outside a command-substitution
2. I don't want `(` and `)` to be treated differently from each other
3. Given the above 2, I would need to use something like `$( ... $)`, however, that looks weird.

Also note that issues 1 and 3 still apply even if I only used 1 character like `|` instead of the parenthesis.

### Reserved Characters

I've limited the number of reserved/special characters quite a bit.  It's possible to have only 1 special character, however, that seems to be too far in one direction.  This makes the source code littered with the "special character" and seems to make the code harder for humans to parse, and consequently, harder to write "correct code".  Minimizing the number of special characters simplifies the language and makes it easier to reason about what the source code is doing, however, special characters can sometimes aid in making the source code more readable.  The strategy I've adopted is to keep special characters limited by default until it becomes clear that adding a new special character is the best way to make it easier to write correct code.  I think the comment character `#` has passed this criteria.  Using `#` to create comments rather than `$#` makes the difference between code and comments easier to distinguish.  It's also a commonly used comment character so it makes the script more compatible with other existing languges which includes working with "shebang line" without special handling for the first line of the script.

### Keywords

I should consider whether it is better to require all builtin's to be qualified with `$` (like `$echo`) or whether I should introduce keywords.  Adding keywords can make it easier to read and write code, however, it adds some cognitive load in that the programmer must remember the keywords, and in rare cases how to create a string that matches a keyword.  This cognitive burden may be small, but it should be compared to the alternative which is when there are no keywords, there is nothing to remember and it is immediately apparent whether something is a builtin.

I want to make a distinction between "incorrect code" and "buggy code".  Incorrect code due to a syntax or semantic error is better than "bugggy code".  Buggy code is code that "sometimes works", but "incorrect code" always fails.  This language is designed to avoid "buggy code", but is less concerned about avoiding obviously "incorrect code".  Incorrect code is still a concern, but sometimes this is in conflict with language simplicity.  For example, adding keywords might make it easier to avoid incorrect code because of a missing `$` in some cases, however, that is obviously incorrect code that can be checked before executing it.  The complexity cost of adding keywords needs to be weighed against this.  Also note that I'm favoring reading code over writing code, so making something more readable takes priority over how hard it is to type.

### Custom Special Character

Maybe a script or line should be able to change their special symbol from `$` to something else?  For example, you could have something like this `$->! !echo your balance is $1.25`.  The single-line case may be tenuous, but if there is a domain where the `$` symbol needs to be escaped alot, maybe allowing it to be subtituted is worthwhile.
