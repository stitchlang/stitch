
# The Stitch Interpreter

### 1. No Runtime Dependencies

The interpreter should be compilable to an executable with no runtime dependencies.  There should be a statically compiled version available.  Package managers are free to add dependencies to create a smaller binaries.

### 2. Written in C

For now I think the best choice of langauge is C.  C is ubiquitous and the language is stable.

### 3. Scalable

The interpreter should be configurable such that minimalistic systems can configure an interpreter to fit in their environment without hindering normal systems from having access to the full power of stitch with as many builtins as they desire.

### 4. Plugins

Users should be able to implement their own builtins.  This would be a shared library that would look something like this:

`stitchplugin.h`

```c
#define STITCH_ABI_VERSION 3
struct stitch_loader {
    // insert fields here
};
```

`myplugin.c`
```c
#include <stitchplugin.h>

int stitch_load_plugins(int version, struct stitch_loader *loader)
{
    if (!loader)
    {
        return STITCH_ABI_VERSION;
    }

    // now we can load our own builtins
    // ...
}
```

I should use the same API to load plugins both dynamically and statically (like kernel modules).  This would allow systems to configure stitch with as many or as little builtins as they want and also allow those builtins to be added dynamically at runtime.

### 5. Using builtins/plugins

With this amount of configuration, there needs to be an easy way for a script to declare what builtins they need, and how to load them if necessary.  For this I imagine a builtin like `@import`

```
@import core
@import os
@import fs
@import (@sharedlibname @scriptdir/mystitchplugins/foo)
```

Maybe selective imports as well like this:

```
@import path dirname join basename
```

With these combined features, maybe the language limits itself to only a few builtins, and the rest are imported?  This could remove the need for prefixing all the builtins with `@`, only the fundamental builtins need the prefix.
