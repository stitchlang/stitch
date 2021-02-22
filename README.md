A scripting language to make it easy to write programs that are correct.  This is an attempt to create a better alternative to languages like BASH.

The purpose of this language is to create programs by "stitching" together other programs (maybe "stitch" is a good name for the language?).  For programs that mainly implement new logic rather than calling other programs, another langauge like Python is better suited. Given this, the basic syntax of the language is:

```
program args...
program args...
```

The "program" string can be:

* a builtin program (like `$echo`)
* a filename (if it contains any slash `/` characters)
* otherwise, it is a program name (a file in one of the `PATH` directories)

> NOTE: if I implement logic to find a program in `PATH` then it needs to be exposed via a builtin such as `$findprog NAME`

Also note that when looking at a script, if something looks like a single argument, it should be.  For example, `$myfile` should always be a single argument whether or not it contains whitespace.  This is achieved by performing variable expansion after program arguments have been separated.  To create mutiple arguments from a variable, array expansion is required.

Special characters:

Char | Description
-----|-------------
`#`  | a single-line comment, escape with `$#`
`$`  | starts an expression, escape with `$$`
`(`  | start a command substitution, escape with `$(`
`)`  | ends a command substitution, escape with `$)`

> NOTE: Say a line starts with `$foo`, if foo is a string, then it's expanded and it's interpreted as a program. If it's a builtin (like $echo), then it's interpreted as that builtin program.

> IDEA: maybe a script or line should be able to change their special symbol from `$` to something else?  For example, you could have something like this `$->! !echo your balance is $1.25`.

## Sample

```
# this is a comment

$echo hello

# variables
$set name Fred
$echo Hello $name

# command substitution
$set arch (uname -m)

# arrays
$setarray names args Fred Lisa Joey
$echo Hello $expand.names
```

> NOTE: $echolines is just like $echo except it prints a newline after each argument
```sh
> $echolines arg1 arg2 arg3
arg1
arg2
arg3

> $echolines arg 1 arg 2 arg 3
arg
1
arg
2
arg
3

> $echolines $@"arg 1" $@"arg 2" $@"arg 3"
arg 1
arg 2
arg 3

> $echolines arg$sp$1 arg$sp$2 arg$sp$3
arg 1
arg 2
arg 3

# NOTE: I can use command substituion to include spaces in a string like this
> $echolines ($echo arg 1) ($echo arg 2) ($echo arg 3)
arg 1
arg 2
arg 3

# awk is a good example to demonstrate because it also makes use of $
> awk $@"{print $1 $2}"

```

# Variables

```
#
# $set [SCOPE.]VARNAME VALUE
#
$set msg Hello, my name is Fred
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

## Arrays

Arrays are important because they provide a way to represent one or more strings without requiring them to be delimited.  Not having an array type and requiring a delimiter instead is the source of many pitfalls with other scripting languages.

```
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
$args-to-lines $expand.mounts

# Not using $expand.VAR will cause an error
$args-to-lines $mounts
# Error: mounts is an array, you must expand it with $expand.mounts
```

Note that arrays cannot be used within another string.  They must be expanded on their own.  The reason for using the `expand` scope to expand arrays, is so that it's immediately apparent that an array is being expanded into 0 or more arguments.

So we have 2 types, strings and arrays of strings. Is that all we need?
We also have scopes, but so far string/arrays/scopes haven't had any intermixing in the grammar.

### Environment variables:

Environment variables are accessed in the `env` scope.

```
$env.VAR

$set env.VAR VALUE
```

## Command Substitution

```
# run command and return one line of output as a string with no trailing newline
(PROG ARGS...)

# run the command and return any number of lines of output
($multiline PROG ARGS...)
```

It's assumed that Command Subtitution is more commonly used to return strings that don't contain newlines.  Because of this, by default Command Substitution only allows up to one line of output from stdout of the underlying command.  It also strips the trailing newline from the output before returning the string.

This default behavior is overriden by prefixing the comand with `$multiline`.  This will return stdout of the underlying process unmodified.

```
$set arch (uname -m)

$set myfile_content ($multiline cat myfile)

$set lsfile (which ls)

# TODO: maybe have a $firstline/$lastline?

# Example
make -j(nproc)
```

> TODO: consider ($firstline COMMAND...) and ($lastline COMMAND...)

# Examples

### Hello World

```
$echo Hello World
```

### Read input and print it

```
$set input (read)
$echo You entered: $input
```

### Random

```
$set pwd (pwd)
$set arch (uname -m)
$set target $arch$-linux-musl
$set prefix $env.HOME/$target
$set jobs -j(nproc)

wget example.come/$target$.tar.xz
tar xf $target$.tar.xz

$set env.PATH $pwd/$target/bin:$env.PATH
```

## Builtin Programs

#### $multiline PROG ARGS...

Runs the given program output wrapped in an internal "MultilineResult" object that allows strings with mulitple lines to be returned from a command-substitution.

## Raw String Mode

I think there is value WYSIWYG strings. They are easier for humans to verify and make it easy to copy strings to and from your script. This means we need a way to disable our special characters `#`, `$`, `(` and `)`.

We can reserve a special character to appear after `$` to enter this "raw string mode".  Note that once in this mode, we will also need a way to escape it, however, we don't want to tie ourselves to one character to escape this mode because the string being represented may need that character.  We solve this by including the sentinel character as well. For example, if we chose `@` to enable raw string mode, then we could do this:

```
$echo $@" I can use #, $, ( and ) in here but not a double-quote "
$echo $@' I can use #, $, (, ) and " but not single-quote '
$echo $@| I can use #, $, (, ), " and ' but not a pipe character in here '
```

As a shorthand, I could also reserve multiple characters for this, like:
```
$echo $" example 1 "
$echo $' example 1 '
$echo $| example 1 |

# probably don't do $( ... ) because that could easily get confused with command-substitution
# maybe just $"..." and $'...'
```

I could also support `"..."`.  This would make the double-quote `"` character a special reserved character. Would this make it easier to write correct programs? Keeping the number of reserved characters low makes it simpler to reason about what source is doing.  The question is whether that benefit outweighs needing to type `$"..."` instead of `"..."`.

Note that the same reasoning that applies to WYSIWYG strings would also apply to HEREDOC strings.  Also, we may raw and processed multiline strings.  A special dollar keyword could start a heredoc, tell if it is raw or processed, then specify the sentinel delimiter.


# Grammar

What I've got so far
```
Script ::= Command*

Command ::= Arg*

Arg ::= (Char | '$' DollarExpr)*

DollarExpr ::= '(' Command ')' | [a-zA-Z_.]* '$'?
```

# Idea

How to handle return code and stdout/stderr?  I could have a CommandResult type of object.
```
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

Piping is a pretty useful feature.  You could emulate piping by setting output to variables, however, piping directly from one process to another could remove extra copies of the output.

A running process has 2 objects, the stdout and stderr file stream.  Once the process has finished, it returns 3 objects, a stdout string, a stderr string and an exit code.

A script should be able to forward stdout and/or stderr from one process to the stdin of another.  Note that to forward stdout/stderr to a named file that can be read by another program, pipe files can be used.  This means the scripting language would need a way to redirect stdout and/or stderr to a named file as well.  Note that this could be done using a pipe operator that goes to a builtin command that forwards it to a file (means we don't need a redirect feature).

Syntax
```

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

```
$cwd DIR PROG ARGS...

# example
$cwd $scriptdir git status

# here's the same example not using CWD, if the program supports it, then this is probably preferred
git -C $scriptdir status
```

# TODO:

* as a test, I should run a script, output all the commands that were run to another script, then run that script a second time and it should still work.  Assuming there is a `--dump-commands-to FILE` command-line option:

```
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
