# stitch

A scripting language that creates programs by "stitching" other programs together.  An alternative to other languages like BASH with a focus on making it easy to write "correct programs".

# Preview

```sh
# this is a comment
@echo Hello, World

# variables
name = Fred
@echo Hello $name

# if statements and inline commands
@if @haveprog uname
    arch = (uname -m)
    @echo $arch
@end

# arrays
names = (@array Fred Lisa Joey)
@echo Hello @expand.names
```

stitch scripts primarily consist of commands containing one or more arguments.  The first argument is inspected to determine how the command is invoked; it can be:

* a builtin program (like `@echo`)
* a program filename (if it contains any slash `/` characters)
* a program name (a file in one of the `PATH` directories)

# Argument Separation Before Expansion

This design choice comes from recognizing that if something looks like a duck, it should be a duck.  A common pitfall of other scripting languages is that program arguments are not always separated as they appear to be in the script.  For example, the command `cat $myfile` would call `cat` with any number of arguments depending on the contents of `$myfile` even though it appears like it is a single argument.  In stitch, this would always be a single argument.  This is accomplished by separating arguments before variable expansion.

> NOTE: in the case where `$myfile` was actually meant to be any number of arguments, array expansion can be used `cat @expand.myfiles`.

# Special Characters

These are the special characters that cause stitch to deviate from the normal mode of specifying programs/arguments:

Char | Description
-----|-------------
`#`  | a single-line comment
`@`  | start a builtin expression and/or access a builtin object
`$`  | access a user object
`"`  | delimit a string literal
`'`  | delimit a string literal
`(`  | start an inline command
`)`  | end an inline command
`=`  | the assignment operator

All of the special characters can currently be escaped with `@X` (where X is the character).  They can also be used inside quoted strings where they have no special meaning.

> NOTE: do I need the `@X` escapes?  why not just use "X"?

# Strings

Most things in stitch evaluate to `String` objects.  The line `foo bar` becomes 2 String objects "foo" and "bar".  This behavior is overriden when special characters are encountered.  Quoted string literals can be used to create strings with these special characters:

```
# printing a normal string

@echo foo
# prints:
#   foo

# using the '$' special character

foo = bar
@echo $foo
# prints:
#   bar

# using a quoted string to disable the '$' special character variable expansion

@echo "$foo"
# prints:
#   $foo

# using a single-quoted string to disable the double-quoted string behavior

@echo '"$foo"'
# prints:
#   "$foo"
```

Quoted string literals are WYSIWYG.  There are no escape sequences and there is no variable expansion. The string literal is opened and closed with the same sequence of quote characters.  Quoted string literals can be interpolated with variables/expressions by omitting whitespace between them:

```sh
name = joe
age = 54
message = "your name is "$name" and your age is "$age
@assert $message @eq "your name is joe and your age is 54"
```

Another technique for interpolation is an inline `@echo` command:

```sh
message = (@echo your name is $name and your age is $age)
```

If the string literals contains double-quote characters then single-quotes can be used:

```sh
@echolines 'fred "ran" a marathon'
# prints:
#   fred "ran" a marathon
```

single-quoted string literals also support multiple delimiters, and once there are at least 3, newlines are also allowed

```sh

content1 = ''now I can have "individual" 'single-quotes' in my string''
content1 = '''a string with "double-quotes" and ''single-quotes'''''
content = '''

# here's a multiline string

'''

> NOTE: if a multiline string literal begins with a newline, it is omitted from the string

```

String interpolation still works with multiline strings

```sh
title = "My Webpage!"
body = "<h1>Hello!</h1>"
@echo '''
<html><head/>
    <title>'''$title'''</title>
</head><body>
    '''$body'''

</body></html>
'''
```

# Arguments with special characters

```sh
# awk is a good example to demonstrate because it also makes use of $
awk "{print $1 $2}"

awk '{print "$1" "$2"}'
```

# When not to use stitch

stitch's primary purpose is to call other programs, when this is not the primary purpose of a script, another language like Python is better suited.

# Type System

stitch has a basic type system with the following object types:

| Name     |  Examples      | Description                                                                 |
|----------|----------------|-----------------------------------------------------------------------------|
| String   | `foo` `"bar"`  | a sequence of characters, this is the default type of most tokens in stitch |
| Array    | `(@array a b c)` | an array of Strings |
| Builtin  | `@echo` `@set` | a "builtin program" |
| Bool     | `@true`        | used by binary expressions like `@true @and @false` and with `@assert @true` |

> NOTE: Internally I also use an Error object with subclasses for different kinds of errors.  Not sure if this will be exposed to stitch scripts yet.  I also have an object for BinaryOperator along with subclasses, also not sure if this will be exposed to stitch scripts.

# Variables

```sh
#
# VARNAME = VALUE
#
msg = "Hello, my name is Fred"
@echo $msg
# prints "Hello, my name is Fred"

cpu_count = (nproc)

@echo You have $cpu_count cpus
@echo Expand cpu count with a suffix is $cpu_count$cpus
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
env    | Environment variable scope.
expand | Scope to expand arrays through.

> NOTE: maybe support @XX@ to insert arbitrary hex characters?  So a space could be @20@?

# Arrays

Arrays are important because they provide a way to represent one or more strings without requiring them to be delimited.  Not having an array type and requiring a delimiter instead is the source of many pitfalls with other scripting languages.

```sh
#
# VARNAME = (@array VALUES...)
# VARNAME = (@splitlines COMMAND...)
#
# Example:
mounts = (@splitlines cat /proc/mounts)
# now the "mounts" variable is an array of all the mounts

# How to use the array in a command?
@echolines @expand.mounts

# Not using @expand.VAR will cause an error
@echolines $mounts
# Error: mounts is an array, you must expand it with @expand.mounts
```

Note that arrays cannot be used within another string.  They must be expanded on their own.  The reason for using the `expand` scope to expand arrays, is so that it's immediately apparent that an array is being expanded into 0 or more arguments.

# Environment variables:

This is subject to change, but for now I've just added some builtins to manage environment variables:

```sh
#
# @env VARNAME
#
#     return the value of environment variable VARNAME, if VARNAME is not set then an error is thrown
#
@echo your PATH is (@env PATH)

#
# @setenv VARNAME VALUE
#
#    set environment variable VARNAME
#
@setenv PATH (@env PATH):/home/mycoolprograms/bin

#
# @unsetenv VARNAME
#
#    unset environment variable VARNAME
#
@unsetenv PATH

#
# @envdefault VARNAME DEFAULT_VALUE
#
#    returns the value of environment variable VARNAME if it exists, otherwise, returns DEFAULT_VALUE
#
@setenv PATH (@envdefault PATH /bin:/usr/bin)
```

I've chosen to use builtins for the time being instead of adding extra syntax because builtins are easy to add/experiment with.  The following shows some more ideas that could be done that require new syntax or new semantics that aren't implemented yet.

```
@echo your PATH is $env.PATH
@echo your PATH is @env.PATH
@echo your PATH is $$PATH
@echo your PATH is $%PATH

env.PATH = foo
```

# Inline Commands

"Inline Commands" are commands contained within other commands.  They are delimited by `(` parenthesis `)`.  By default, inline commands return a `String` created from capturing stdout of the underlying command.  

> NOTE: inline commands are executed within the current process environment, they are not executed in a subprocess

```sh
# run command and return one line of output as a string with no trailing newline
(PROG ARGS...)

make -j(nproc)

arch = (uname -mm)
@echo arch is $arch

files_exist = ((@isfile foo) @and (@isfile bar))
```

### @multiline

In general, inline commands are commonly used to return strings that don't contain newlines.  Because of this, by default, inline commands only allow up to one line of output from stdout of the underlying command.  It also strips the trailing newline from the output before returning the string.  To disable this behavior, the `@multiline` builtin can be used to prefix the command:

```
# run the command and return any number of lines of output
(@multiline PROG ARGS...)

myfile_contents = (@multiline cat myfile)
```

### Capturing exitcode and/or stderr

The following examples show how to prefix an inline command to change whether the exitcode, stdout and/or stderr are captured.

```sh
foo = (COMMAND)
# exitcode: non-zero is fatal
# stdout: captured
# stderr: not captured
# foo is a String of COMMAND's stdout

foo = (@exitcode COMMAND)
# exitcode: captured
# stdout: not captured
# stderr: not captured
# foo is a Bool based on COMMAND's exit code, 0 (success) maps to true, and non-zero maps to false

# NOTE: maybe I add @exitcodeint for capturing the exitcode as a String?

foo = (@stderr COMMAND)
# exitcode: non-zero is fatal
# stdout: not captured
# stderr: captured
# foo is a String of COMMAND's stderr

foo = (@stdout @stderr COMMAND)
# exitcode: non-zero is fatal
# stdout: captured
# stderr: captured
# foo is a CommandResult object with the "out" field set to stdout and the "err" field set to stderr

foo = (@exitcode @stderr COMMAND)
# exitcode: captured
# stdout: not captured
# stderr: captured
# foo is a CommandResult object with the "exitcode" field set to the exit code and the "err" field set to stderr

foo = (@exitcode @stdout @stderr COMMAND)
# exitcode: captured
# stdout: captured
# stderr: captured
# foo is a CommandResult object with the "exitcode" field set to the exit code, the "out" code set to stdout and the "err" field set to stderr
```

> NOTE: multiline is probably going to be incompatible with these variations since I think it will be more common to keep newlines when stderr is captured.  Note that enforcing a single line can also be done after the fact if wanted.  So right now if the interpreter were to see `(@multiline @stderr COMMAND)`, then it would assert a Semantic error saying `@multiline and @stderr are redundant`.

# Examples

### Hello World

```sh
@echo Hello World
```

### Read input and print it

```sh
input = (read)
@echo You entered: $input
```

### Random

```sh
pwd = (pwd)
arch = (uname -m)
target = $arch$-linux-musl
prefix = @env.HOME/$target
jobs = -j(nproc)

wget example.come/$target$.tar.xz
tar xf $target$.tar.xz

@setenv PATH $pwd/$target/bin:(@env PATH)
```

# Builtin Programs

Because this scripting language uses named builtin's, this enables the language to provide many features without new syntax.  Among these features include cross-platform builtin programs.  The following questions should be considered for new builtin programs:

1. How simple is the builtin program?
2. How common/generally useful is the builtin program?

If a feature is "simple" and provides some sort of benefit, then there's not much downside to including it in the language.  A few lines of code is all that's needed to include it in the language.  The only downside I see is feature overlap/multiple ways to do the same thing which should be avoided.

If a feature is not so simple, then it still may be a candidate for inclusion if it is common and/or generally useful enough.

# Builtin Argument Expansion

Builtin programs accept arguments that have been expanded to various levels:

1. AstNodes
2. ExpandNodesResult
3. Objects (Bool, String, Array, etc)
4. Strings

At the lowest level 1 we have AstNodes.  These arguments are left in their unprocessed form where they have been categorized into Tokens, Variables and Inline Commands. Builtins like `@note`, `@multiline` and `@call` accept AstNodes.  Binary Operators also keep their operands in AstNode form to support "shortcircuting" where the right operand may not be expanded if the final result is already determined from the left operand (note: this is disabled in verification mode).

Skipping past level 2 for now, we have level 3 "Objects" that are accepted by builtins like `@settmp`, which takes a `String` for its first argument, and either `String`, `Bool` for for its second along with a command.

At the final level we have "Strings".  This is the same form that external programs accept and also builtins like `@echo`.

The following lists some of the Builtins and the argument form they take:

```
@note AstNode...
@echo String...

@settmp String (String|Bool) AstNode...

@multiline AstNode...
@call String AstNode...

@if ExpandNodesResult
@assert ExpandNodesResult
@not ExpandNodesResult
```

At level 2 we have the `ExpandNodesResult` form.  This is an internal form of the arguments that the default command handler creates before deciding what to do next with the command.  Its is a union type with the following cases:

* `ExpandNodesResult.Builtin`
* `ExpandNodesResult.BinaryExpression`
* `ExpandNodesResult.Bool`
* `ExpandNodesResult.ExternalProgram`

Currently `@assert` and `@not` take this form because it allows them to either interpret 1 object as a `Bool`, or multiple arguments as a command or expression to expand and retrieve the result.  Supporting multiple arguments means we don't have to use parenthesis when they are not necessary:

```
# instead of this
@assert (@not (@isfile $f))

# we can do this
@assert @not @isfile $f
```

# List of Builtin Programs

> NOTE: this list is sorely lacking and out-of-date, will work on this later when language is more refined

#### @multiline PROG ARGS...

Modifies an inline command to return multiple lines.

#### @firstline/@lastline

Not sure if these should be added yet. I've included them to be considered.

#### @findprog NAME

Assuming programs are located the same way as BASH and/or execvp, I should expose this logic through a builtin.

# String Literals

> NOTE: this section is old, need to combine with the "Strings" section

WYSIWYG strings make it easier to write correct code because they are easy for humans to verify and copy between applications. Requiring manual edits to escape special characters on strings copied to and from stitch scripts would be error prone. To avoid this, we need syntax to disable special characters like `#`, `@`, `$`, `"`, `(` and `)`.

For this stitch supports both "double-quoted string literals" and 'single-quoted string literals':

```sh
@echo "I can use #, @, $, ( and ) in here but not a double-quote"
# prints:
#   I can use #, @, $, ( and ) in here but not a double-quote

@echo 'I can use #, @, $, (, ) and " but not single-quote'
# prints:
#   I can use #, @, $, (, ) and " but not single-quote

@echo ''I can use #, @, $, (, ), " and ' but not 2 consecutive single-quotes''
# prints:
#   I can use #, @, $, (, ), " and ' but not 2 consecutive single-quotes

@echo '''I can use #, @, $, (, ), " and '' but not 3 consecutive single-quotes'''
# prints:
#   I can use #, @, $, (, ), " and '' but not 3 consecutive single-quotes
```

If a single-quoted string starts with at least 3 single-quotes, then it also supports newlines

```sh
@echo '''
this
is
a multiline
string literal
'''
```

### Idea: detect and assert error when strings are concatenated with no interpolation

Imaging the following:
```
@echo "this is a "string" with double-quotes"
```

This will be interpreted as:

```
@echo "this is a "string" with double-quotes"
      |          ||    ||                   |
       ----------  ----  -------------------
        String    String      String

```

which would be printed this:
```
this is a string with double-quotes
```

What likely happened here is the user copied a string and pasted it into stitch and then stitch's string literal syntax messed it up.  This is incorrect code but we can actually detect at "verification time" by detecting when there are consecutive string literal nodes.  We could make this a syntax error which would give us this:

```sh
"foo"bar          # syntax error: replace "foo"bar with foobar
"foo""bar"        # syntax error: replace "foo""bar" with foobar
"foo"@%"bar"      # syntax error: replace "foo"@%"bar" with foobar

"foo"$bar         # OK
"foo"(@echo bar)  # OK
"foo"@"           # OK
```

> NOTE: should it be an error to quote something that doesn't need to be quoted? "foo" must be foo?

# Binary Expressions

```sh
#
# Binary Expression Syntax:
#
Node BinaryOperator Node


# OK
$a @and $b

# Syntax Error: binary expressions only accept single-node operands
grep foo bar @and $b

# OK
(grep foo bar) @and $b
```

Currently we have a few binary operators, `@and`, `@or`, `@eq`, `@gt` and `@lt`.  Here are some more candidates:

```sh
Node @equals Node
Node @lessorequal Node
Node @greaterorequal Node
Node @less Node
Node @greater Node

# or maybe
Node @= Node
Node @== Node
Node @<= Node
Node @>= Node
Node @< Node
Node @> Node
```

Note that binary expressions always return `Bool` values.  This means that if they appear inside an "inline command", then the return channel is already used up, so boolean expressions always disable capturing stdout when inside an inline command.  They also don't work with command modifiers like `@exitcode`, `@stderr`, `@multiline`, etc.

### Short Circuting

If the final result of a binary expression has been determined before it has been fully evaluated, the language does not expand the rest of the expression.  Because of this, commands must be checked for binary operators before expanding them.

### Binary Expression TODO/Questions

* Top Level Handling?

How should Boolean objects be handled at the top-level?  For now I've just made them an error "uhandled Bool".  `@assert` can be used to handle them by asserting they are true, and `@assert @not ...` can handle assertig they are false.

* Unary Expressions?

I don't think the language needs an special handling for unary expressions.  I believe these can be handled by builtins, like:

```sh
@exists PATH
@isdir PATH
@isfile PATH
```

# Ambiguous Operators

Currently the only ambiguous operator is `@not` (will probably be more later).  During expansion, stitch tracks when a command is inside an ambiguous operator (like `@not`) and will raise a SemanticError if it encounters a BinaryExpression.  This is to prevent ambiguous commands like this:

```sh
@not $foo @and $bar
```

The ambiguity can be corrected with one of these 2 variations:
```
(@not $foo) @and $bar
@not ($foo @and $bar)
```

# Control Flow

### First Idea for if/while:

```sh
#
# if/elif/else/endif
#
@if command
    command
    command
    ...
@elif command
    command
    command
    ...
@else
    command
    command
    ...
@end

#
# while/continue/break
#
@while command
    command
    command
    ...

    @if command
        @continue
    @if command
        @break

@end
```

# Grammar

What I've got so far
```
Script ::= Command*

Command ::= Argument* | BinaryExpression

Argument ::= (Char | '@' AtExpression | '$' DollarExpression)* | BinaryExpression

DollarExpression ::= '(' Command ')' | [a-zA-Z_.]* '$'?

# TODO: AtExpression
#AtExpression ::= ...

BinaryExpression ::= Operand Op Operand ( Op Operand )*

Operand ::= '(' Command ')' | Argument
```

> NOTE: "BinaryExpression" takes "Operand" instead of "Argument"  because these nodes are evaluated differently

# Idea: piping

UPDATE: I think piping *might* be a binary operator, so I need to update this section to reflect how it would work with binary operators

Piping is a pretty useful feature.  You could emulate piping by setting output to variables, however, piping directly from one process to another could remove extra copies of the output.

A running process has 2 objects, the stdout and stderr file stream.  Once the process has finished, it returns 3 objects, a stdout string, a stderr string and an exit code.

A script should be able to forward stdout and/or stderr from one process to the stdin of another.  Note that to forward stdout/stderr to a named file that can be read by another program, pipe files can be used.  This means the scripting language would need a way to redirect stdout and/or stderr to a named file as well.  Note that this could be done using a pipe operator that goes to a builtin command that forwards it to a file (means we don't need a redirect feature).

Syntax
```sh

foo @out bar
foo @err bar
foo @outerr bar

# How to pipe stdout and stderr to different places?

# the following won't work because @err could belong to the previous command
foo @out COMMAND... @err COMMAND...


# to redirect the output of "foo" to a file "myfile", use @in2file
foo @out @in2file myfile

# to forward out/err to different places, could do multiple lines like this
foo = @stage foo args...
# the @stage COMMAND... builtin will create create a process that has not been started yet
# now $myprogram.out and $myprogram.err are open file handles
@attach $foo.out bar
@attach $foo.err baz
@run $foo
```

NOTE: I think I should implement multi-command semantics for piping first.  This will help me setup the proper abstractions for piping before I tacke the interface for the shorthand version.

Here's what piping might look like if I treated it as a binary operator:

```sh
(ps axo pid,args) @pipe (awk '{print $1, $2}') @pipe (grep '[ /]'$program'$') @pipe (awk '{print $1}')
```

# The Current Working Directory

Having a current working directory as hidden state that affects all relative path names may be more trouble than it's worth.  One alterative is to use absolute path names.  However, some programs use the current working directory as an important input, in which case there needs to be a way to set it.  Here's a way we could set this and make it excplicit:

```sh
@withcwd DIR PROG ARGS...

# example
@withcwd @scriptdir git status

# here's the same example not using CWD, if the program supports it, then this is probably preferred
git -C @scriptdir status
```

# Script Verificaton

"Script Verification" means checking a script for syntax/semantic errors before executing it.  This verification is done on all the code inside the script, not just the parts executed on a particular invocation.  This verification is vitally important in making it easy to write "correct code" because catching all "execution errors" requires exhaustive testing, but catching all syntax/semantic errors can be done without any tests.  This is a drastic distinction between these two kinds of errors.  When evaluating language features, making "execution errors" into "syntax/semantic errors" has a big affect on making it easier to write correct code.

By default, "Script Verification" is performed on every script before it is executed.  To disable this verification, a script can include the `@noverify` builtin to stop verification wherever it appears.  It also takes an optional md5 hash that the interpreter will use to verify the contents of the rest of the script.  This provides a way for a script disable verification only if the script hasn't changed.

```sh
# disable verification
@noverify

# disable verification only if the rest of the file matches this hash
@noverify 03299287e43dc67cf0f177aa85031a43
```


### Idea Embedded Programs

This would be the analogue to "Bash Functions"

```
@program foo
    @echo this is the "foo" program
@end
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

### Why Inline Commands are `(...)` rather than `@(...)` or `$(...)`

This syntax is a result of the following 3 observations:

1. I don't want `)` to be treated differently inside or outside an inline command
2. I don't want `(` and `)` to be treated differently from each other
3. Given the above 2, I would need to use something like `@( ... @)` or `$( ... $)`, however, these look weird

I should also consider reserving a single character such as `|`, then have `@|` mean mean "start command" and `|` mean "end command".  So it would look like this `@| ... |`.  This solves issue 1, issue 2 doesn't apply because it's a single character, and it doesn't look weird.  This would also mean only reserving 1 character instead of the 2 parenthesis characters `(` and `)`.  I'll have to compare these solutions.

### Reserved Characters

I've limited the number of reserved/special characters quite a bit.  It's possible to have only 1 special character, however, that seems to be too far in one direction.  This makes the source code littered with the "special character" and seems to make the code harder for humans to parse, and consequently, harder to write "correct code".  Minimizing the number of special characters simplifies the language and makes it easier to reason about what the source code is doing, however, special characters can sometimes aid in making the source code more readable.  The strategy I've adopted is to keep special characters limited by default until it becomes clear that adding a new special character is the best way to make it easier to write correct code.  I think the comment character `#` has passed this criteria.  Using `#` to create comments rather than `@#` makes the difference between code and comments easier to distinguish.  It's also a commonly used comment character so it makes the script more compatible with other existing languges which includes working with "shebang line" without special handling for the first line of the script.

#### Development Note: Adding `@`

I originally used `$` to access all symbols, both language-defined and user-defined.  @ifruend suggested that accessing language symbols through another character like `@` would make stitch more readable.  Doing this elevates the distinction between user/language symbols to the syntax itself rather than an implementation detail.  I agree that being able to determine a symbol's origin makes things more readable, enough to warrant a new special character, so I accepted this change.

Also note this this feature alleviates the readability benefit that keywords would add.

### Keywords

I should consider whether it is better to require all builtin's to be qualified with `@` (like `@echo`, `@if`, `@and`) or whether I should introduce keywords.  Adding keywords can make it easier to read and write code, however, it adds some cognitive load in that the programmer must remember the keywords, and in rare cases how to create a string that matches a keyword.  This cognitive burden may be small, but it should be compared to the alternative which is when there are no keywords, there is nothing to remember and it is immediately apparent whether something is a builtin.

I want to make a distinction between "incorrect code" and "buggy code".  Incorrect code due to a syntax or semantic error is better than "bugggy code".  Buggy code is code that "sometimes works", but "incorrect code" always fails.  This language is designed to avoid "buggy code", but is less concerned about avoiding obviously "incorrect code".  Incorrect code is still a concern, but sometimes this is in conflict with language simplicity.  For example, adding keywords might make it easier to avoid incorrect code because of a missing `@` in some cases, however, that is obviously incorrect code that can be checked before executing it.  The complexity cost of adding keywords needs to be weighed against this.  Also note that I'm favoring reading code over writing code, so making something more readable takes priority over how hard it is to type.

### The `=` character

I originally didn't have a special syntax for assigning variables, it use to be this:

```sh
@set foo bar
@set arch (uname -m)
```

But it seems like adding a special character `=` for variable assignment is worth both the special character and special syntax for readability.
```sh
foo = bar
arch = (uname -m)
```

Note, I'm not sure but maybe this syntax would be more intiuitive?

```
$foo = bar
$arch = (uname -m)
```

### Idea: Multiple debug/log formats

Maybe stitch should support logging commands using multiple output formats (like BASH, etc).  The problematic part of this would be the builtins, one way to address this would be to also output the builtins required as they are being used. Another idea would be to provide a BASH script that provides all the builtins as BASH functions.

### Syntax vs Semantics

I have a choice to impelement binary operators like `@and` and `@eq` either though syntax or through semantic analysis.  In general it "seems" like if it's possible to represent something in the syntax, then it should be done.  The syntax layer is simpler so if a feature can be moved to that layer then it seems like it could make the language simpler overall.  I'm not sure if this is correct in all cases though, it's just an intuitiion at the moment.  On the other hand, if I were to put binary operats in the syntax, then it would be much more difficult to add new binary operators, as that would have to be done at the syntax layer, something which is typically immutable in a langauge.  So maybe if something is going to by dynamic/configurable, then it should go into the semantic layer, otherwise, it should go into the syntax layer if possibe?  Still not sure, I'll need to think about this.  Another candidate for this are constructs like `@if`, `@end`, etc.  This also ties into the keywords and `=` character ideas.  If `@if` is moved to the syntax layer then it could be a keyword `if` and if `=` is chosen for variable assignment then it could also be moved to the syntax layer.  Escaping this behavior could be done with quotes `"if" ...` and `a "=" b`. Then again, having every special thing in the language be prefixed with `@` could make things simpler for programmers, but then again, will programmers ever not know that `=` or `if` are not special?  If I start to introduce keywords and new syntax, the cost for new features becomes much higher because now both the language builtins and the user data are in the same namespace, so introducing new stuff means enroaching on the user namespace and breaking their programs that were using those symbols.  Taking all this into account, I should definitely start without keywords and evolve from there. However, the `=` question if more pressing I think.
