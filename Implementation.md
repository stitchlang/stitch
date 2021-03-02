
# The Stitch Interpreter

### 1. No Runtime Dependencies

The interpreter should be compilable to an executable with no runtime dependencies.  There should be a statically compiled version available.  Package managers are free to add dependencies to create a smaller binaries.

### 2. Written in C

For now I think the best choice of langauge is C.  C is ubiquitous and the language is stable.

### 3. Scalable

The interpreter should be configurable such that minimalistic systems can configure an interpreter to fit in their environment without hindering normal systems from having access to the full power of stitch with as many builtins as they desire.

### 4. Modules

I should consider whether to support modules in stitch.  Modules allow symbols to be categorized which can help know where to look for things and keep the namespace clean. I imagine importing a module could look something like this:

```
@import core
@import os
@import fs
```

Maybe selective imports as well like this:

```
@import path dirname join basename
```

With modules, maybe the language limits itself to only a few fundamenatal builtins, and the rest are imported?  This could remove the need for prefixing all the builtins with `@`, only the fundamental builtins need the prefix.

Note that I would expect most "generally useful" builtins to be included in the `stitch` repository and would get deployed by default.  Excluding builtins would be an explicit decision to create a minimal interpreter.

### User Defined Modules?

We need to find good use cases for this first, but if modules were implemented I imagine stitch could support custom modules through shared libraries:

```
@import (@sharedlibname @scriptdir/mystitchplugins/foo) customFooFunc

customFooFunc
```

Note that stitch is meant to call other "programs" so there would have to be good use cases that custom builtins enable that normal programs couldn't.  A custom builtin would have more information than an external program as it could access the object Types and the state of the interpreter. I imagine a custom module would look something like this:

`stitchmodule.h`

```c
#define STITCH_ABI_VERSION 3
struct stitch_loader {
    // insert fields here
};
```

`mymodule.c`
```c
#include <stitchmodule.h>

int stitch_init_module(int version, struct stitch_loader *loader)
{
    if (!loader)
    {
        return STITCH_ABI_VERSION;
    }

    // now we can load our own builtins
    // ...
}
```

If stitch were to support something like this, then one idea would be to implement all (or most) builtins through this module interface.  This would allow a user to configure stitch with modules statically compiled into the interpreter and one that are dynamically loaded without changing the source code.


### 6. Build Dependencies

I like to have my cake and eat it too.  I want people to be able to be able to use whatever build tools they have on their system, but I don't want to have to manually maintain these build systems separately.  I want to configure the project in one place.  I also don't want to require people to have a specific tool to bootstrap their build system from something else.  So the idea I have is to configure the project in one location, probably a JSON file, and then generate any and all build systems that people might use (meson, cmake, Makefile, autotools?).  I imagine this "build generator" is written in Python, but I don't want to make Python a required dependency, so I'll commit the generated build files to the repository.  To keep these files in sync, the CI system will check that the main configuration file generates the build files the same way they appear within any commit.

> note: the build generator would be tucked away in a subdirectory, not "up front and center" in the top-level directory.  This is because most people won't know or care about it, they will just want to know where their build system files are.
