# stitch

A scripting language that creates programs by "stitching" other programs together.  An alternative to other languages like BASH with a focus on making it easy to write "correct programs".

# Preview

```sh
# this is a comment
@echo Hello, World

# variables
@set name Fred
@echo Hello $name

# if statements and inline commands
@if @haveprog uname
    @set arch (uname -m)
    @echo $arch
@end

# arrays
@setarray names args Fred Lisa Joey
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
`#`  | a single-line comment, escape with `@#`
`@`  | start a builtin expression and/or access a builtin object, escape with `@@`
`$`  | access a user object `@$`
`"`  | delimits a string literal, escape with `@"`
`(`  | start an inline command, escape with `@(`
`)`  | ends an inline command, escape with `@)`

# Arguments with Spaces

```sh
@echolines arg 1 arg 2
# prints:
#   arg
#   1
#   arg
#   2

# How do we use @echolines to print the following instead?
#   arg 1
#   arg 2

#
# 1. use a string literals
#
@echolines "arg 1" "arg 2"

#
# 2. use inline commands
#
@echolines (@echo arg 1) (@echo arg 2)

#
# 3. use a "Delimited String Literal"
#
@echolines @%"arg 1" @%"arg 2"
```

> NOTE: consider adding support for `@_` so you could do `@echolines arg@_1 arg@_2

# Arguments with special characters

```sh
# awk is a good example to demonstrate because it also makes use of $
awk "{print $1 $2}"

awk @%|{print "$1" "$2"}|
```

# When not to use stitch

stitch's primary purpose is to call other programs, when this is not the primary purpose of a script, another language like Python is better suited.

# Type System

stitch has a basic type system with the following object types:

| Name     |  Examples   | Description                                                                 |
|----------|-------------|-----------------------------------------------------------------------------|
| String   | `foo` "bar" | a sequence of characters, this is the default type of most tokens in stitch |
| Array    | `@setarray foo args a b c` | an array of Strings |
| CommandResult | `(@echo hello)` | an exitcode and optional Strings for stdout/stderr if they were captured |
| Builtin  | `@echo` `@set`  | a "builtin program" that takes arguments and returns a CommandResult |
| Bool     | `@true` | used by binary expressions like `@true @and @false` and with `@assert @true` |

> NOTE: Internally I also use an Error object with subclasses for different kinds of errors.  Not sure if this will be exposed to stitch scripts yet.  I also have an object for BinaryOperator along with subclasses, also not sure if this will be exposed to stitch scripts.

# Variables

```sh
#
# @set [SCOPE.]VARNAME VALUE
#
@set msg "Hello, my name is Fred"
@echo $msg
# prints "Hello, my name is Fred"

@set cpu_count (nproc)

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
# @setarray [SCOPE.]VARNAME splitlines VALUE
# @setarray [SCOPE.]VARNAME args ARGS...
#
# TODO: splitwhitespace?
#
# Example:
@setarray mounts splitlines (cat /proc/mounts)

# now the "mounts" variable is an array of all the mounts

# How to use the array in a command?
@echolines @expand.mounts

# Not using @expand.VAR will cause an error
@echolines $mounts
# Error: mounts is an array, you must expand it with @expand.mounts
```

Note that arrays cannot be used within another string.  They must be expanded on their own.  The reason for using the `expand` scope to expand arrays, is so that it's immediately apparent that an array is being expanded into 0 or more arguments.

# Environment variables:

Environment variables are accessed through the `env` scope.

```sh
@env.VAR

@set env.VAR VALUE
```

# Inline Commands

"Inline Commands" are commands contained within other commands.  They are delimited by `(` parenthesis `)`.  Inline commands can return both `CommandResult` objects and `Bool` objects.  How the inline command is handled will depend on where it is being invoked.  If its result is coerced to a `String`, then its exitcode will be checked to have been `0` lest an error be thrown, then its `stdout` will be interpreted as the `String`.

> NOTE: inline commands are executed within the current process environment, they are not executed in a subprocess

```sh
# run command and return one line of output as a string with no trailing newline
(PROG ARGS...)

make -j(nproc)

@set arch (uname -mm)
@echo arch is $arch

@set files_exist ((@isfile foo) @and (@isfile bar))
```

### @multiline

In general, inline commands are commonly used to return strings that don't contain newlines.  Because of this, by default, inline commands only allow up to one line of output from stdout of the underlying command.  It also strips the trailing newline from the output before returning the string.  To disable this behavior, the `@multiline` builtin can be used to prefix the command:

```
# run the command and return any number of lines of output
(@multiline PROG ARGS...)

@set myfile_contents (@multiline cat myfile)
```

#### Idea, diable stdout capture when Bool expected?

I may want to modify inline commands to only capture stdout when the context it is used in requires a `String`.  When a `Bool` is expected, stdout is just going to get forwarded so this is wasting memory and cpu cycles to copy it.

# Examples

### Hello World

```sh
@echo Hello World
```

### Read input and print it

```sh
@set input (read)
@echo You entered: $input
```

### Random

```sh
@set pwd (pwd)
@set arch (uname -m)
@set target $arch$-linux-musl
@set prefix @env.HOME/$target
@set jobs -j(nproc)

wget example.come/$target$.tar.xz
tar xf $target$.tar.xz

@set env.PATH $pwd/$target/bin:@env.PATH
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
3. Objects (Bool, String, CommandResult, Array, etc)
4. Strings

At the lowest level 1 we have AstNodes.  These arguments are left in their unprocessed form where they have been categorized into Tokens, Variables and Command Subtitutions. Builtins like `@note`, `@multiline` and `@call` accept AstNodes.  Binary Operators also keep their operands in AstNode form to support "shortcircuting" where the right operand may not be expanded if the final result is already determined from the left operand (note: this is disabled in verification mode).

Skipping past level 2 for now, we have level 3 "Objects" that are accepted by builtins like `@set`, which takes a `String` for its first argument, and either `String`, `Bool` or `CommandResult` for its second.

At the final level we have "Strings".  This is the same form that external programs accept and also builtins like `@echo`.

The following lists some of the Builtins and the argument form they take:

```
@note AstNode...
@echo String...

# NOTE: set/settmp will coerce CommandResult object to Strings in the symbol table
@set String (String|Bool|CommandResult)
@settmp String (String|Bool|CommandResult) AstNode...

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

Modifies the CommandResult object of an inline command to accept multiple lines from stdout and disables stripping the trailing newline.

#### @firstline/@lastline

Not sure if these should be added yet. I've included them to be considered.

#### @findprog NAME

Assuming programs are located the same way as BASH and/or execvp, I should expose this logic through a builtin.

# Delimited String Literals

WYSIWYG strings make it easier to write correct code because they are easy for humans to verify and copy between applications. Requiring manual edits to escape special characters on strings copied to and from stitch scripts would be error prone. To avoid this, we need syntax to disable the special characters `#`, `@`, `$`, `"`, `(` and `)`.

The solution for stitch is "Delimited String Literals".  They start with the sequence `@%` followed by a delimiter character.  The string continues until it sees the delimiter character again.  Here are some examples:

```sh
@echo @%"I can use #, @, $, ( and ) in here but not a double-quote"
# prints:
#   I can use #, @, $, ( and ) in here but not a double-quote

@echo @%'I can use #, @, $, (, ) and " but not single-quote'
# prints:
#   I can use #, @, $, (, ) and " but not single-quote

@echo @%|I can use #, @, $, (, ), " and ' but not a pipe character in here|
# prints:
#   I can use #, @, $, (, ), " and ' but not a pipe character in here
```

The scripting language should also probably include a way to create multiline strings.  I'll decide on this later.

It might also be good to include some shorthand variations like this:
```sh
@echo @" example 1 "
@echo @' example 1 '
@echo @| example 1 |

# probably don't do @( ... ) because that could easily get confused with inline commands
# maybe just @"..." and @'...'
```

I could also support `"..."`.  This would make the double-quote `"` character a special reserved character. Would this make it easier to write correct programs? Keeping the number of reserved characters low makes it simpler to reason about what source is doing.  The question is whether that benefit outweighs needing to type `@"..."` instead of `"..."`.

Note that the same reasoning that applies to WYSIWYG strings would also apply to HEREDOC strings. A special dollar keyword could start a heredoc, tell if it is raw or processed, then specify the sentinel delimiter.


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

which would be print this:
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

Boolean binary operators like `@or` and `@and` can take a CommandResult object, and convert it to a Bool object based on it's exit code.

When a binary operator receives a CommandResult object from an inline command, it can handle it differently depending on the operator.  For the Bool binary operators like `@or` and `@and`, it converts the exit code of the command to a `Bool` where `0` (success) becomes `true` and non-zero (error) becomes `false``.  In this case, since stdout is ignored, it is printed to the current stdout handler instead (or I might modify inline commands to know beforehand they are going to become `Bool` and never capture it in the first place).

Currently there are only 2 binary operators: `@and` and `@or`.  Here are some more candidates:

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

## Use Cases

> NOTE: this section is old and outdated

```sh
# returns a CommandResult "top-level handler" which causes a fatal error on non-zero exit code
grep needle file

# @if will accept both zero and non-zero exit codes from a CommandResult and use it to decide on the brancht to execute
@if grep needle file
    @echo found needle!
@else
    @echo did not find needle
@end

# unary operator example

# applying a "test operator" to a CommandResult returns a Bool
grep needle file
@not grep needle file

# both of the commands above would cause an "unhandle Bool" if they appeared at the top-level
```

So any command that is given to a "test operator" must bubble up to a control flow builtin like `@if`, `@while`.

```sh
@if @not grep needle file @and @not grep needle2 file
    @echo both needles are not found
@else
    @echo at least one needle is present
@end
```

So, unary operators are always higher precedence than binary operators.  What if we want to override that?

```sh
@if @not (grep needle file @and grep needle2 file)
    @echo at least one needle is missing
@else
    @echo both needls are present
@end
```

I think the example above just WORKS.  Inline commands take any Bool and propogate it up as a Bool.  If a Bool is attempted to be used as a string, it is an error, i.e.

```sh
ls (@not grep needle file)
#
# error: expected a string but got Bool
#
```

What if we required parenthesis around binary operators?

```sh
# maybe this is an error?
@not grep needle file @and grep needle2 file

# need this
(@not grep needle file) @and (grep needle2 file)
```

I think this makes things more clear and readable.  After a command is expanded, binary operators only support one object to their left and right.  If there are more then it's an error.  What about chaining binary operators?

```sh
# OK
foo @and bar @and baz

# This looks confusing
foo @or bar @and baz
```

I think the right thing to do here is to only allow chaining binary operators if they are the same operator.

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

# Idea: stderr/exitcode

How to handle return code and stdout/stderr?  I could have a CommandResult type of object.
```sh
#
# @captureouts PROG ARGS...
# @captureall     PROG ARGS...
#
# the @capture* builtins will run the program/args that follow, and instead of just returning stdout, it will return
# an object.  @capturestreams will capture stdout and stderr via the "out" and "err" fields.  @captureall will also capture
# the exit code in the field `code`, note that this overrides the default behavior of exiting on a non-zero exit code.

@set some_program_result (@captureouts some-program)
@echo the stdout of some_program_result is: $some_program_result.out
@echo the stderr of some_program_result is: $some_program_result.err

@set another_program_result (@captureouts another-program)
@echo the stdout of another_program_result is: $another_program_result.out
@echo the stderr of another_program_result is: $another_program_result.err
@echo the exit code of another_program_result is: $another_program_result.code
```

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
@set foo @stage foo args...
# the @stage COMMAND... builtin will create create a process that has not been started yet
# now $myprogram.out and $myprogram.err are open file handles
@attach $foo.out bar
@attach $foo.err baz
@run $foo
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

Consider making `=` a special character:

```sh
@set foo bar
@set arch (uname -m)
```
Versus
```sh
foo = bar
arch = (uname -m)
```

Which is more readable? There's also this variation:

```
$foo = bar
$arch = (uname -m)
```

With this variation it would have to be clear that `$VAR = VALUE` is not a command, but a different syntax node.  This would complicate the syntax of the language, but that complexity could be outweighed by the "readability" and "familiarity".
