# VisualInterface

This is an F# interface to [VisUAL](https://salmanarif.bitbucket.io/visual/downloads.html) emulator. The basic functionality is a function that has input an assembler source file and output a trace of execution, similar to what would happen in the VisUAL GUI.

A built-in cache, if configured, means that repeated execution of the same program will be near instantaneous. Otherwise every program takes 0.5s or so to execute due to Java startup time.

For information on VisUAL assembly instructions see the VisUAL documentation, or ask on Slack.

## Files

* `.\LibVisual` - F# project containing interface to VisUAL app, compiling to a library (dll).
* `.\visual` - F# project with test code for the LibVisual interface
* `.\visualapp` - the VisUAL app itself, with its portable Java distribution
* `.\ visualWork` - directory used by VisUAL and the interface for temporary files

## Code

Two VS projects are supplied:

1. *Visual* runs VisualInterface with some sample data
2. *LibVisual* compiles VisualInterface to a DLL for reference from other projects

These two projects use the same source code (linked in LibVisual) therefore you must keep them together with the same relative position in the file system.

You can use VisualInterface either by interfacing to the LibVisual DLL or by incorporating the VisualInterface source code directly in your project. In the latter case see Dependencies below.

## Dependencies

VisualInterface has dependencies on NuGet packages:

* FSharp.Data
* FsPickler

These will need to be added to any project that uses the source code directly, rather than the dll.

VisualInterface requires VisUAL to be [downloaded](https://salmanarif.bitbucket.io/visual/downloads.html) and placed in a directory which is input as `paras.VisualPath`. Note that Visual is packaged with a portable version of Java which runs it.

## Operation

```
RunVisual (paras:Param) (src:string)
```

Runs VisUAL on source text `src` - NB `src` is not a filename returning a trace of execution with register values as a `Result[]`. The directory `paras.WorkFileDir` will be used for temporary files and also the persistent (across calls to RunVisual) cache file used to speed up repeated indentical simulations. This can be useful for debugging where the raw output from VisUAL can be found after a run in this directory.

Normal operation is with cache on `paras.cached=true` because the cache mechanism is very robust. If anything goes wrong the cache will be recreated, which slows down operation, but does not affect results. Especially when testing tests, the cache saves considerable time.

`program.fs` in `visual` shows sample usage in which a *postlude* of assembly instructions is appended to the code to be simulated in order to extract the `NZCV` status bit values.

## Visual Memory Map

For convenience (to avoid use of complex postludes) VisUAL allows the values of specified memory locations to be traced and therefore returned. The locations so tracked are specified by `paras.MemLocs`. In addition the Visual memory data section can be confidured to start at `paras.MemStart`. Note that Visual has an exceptionally simple memory model in which code starts at location 0, and that all locations are specified as byte addreses, and therefore must be divisible by 4. For various reasons the data section start point should always be aligned with 256 byte chunks.

Visual allows data directives `DCD`,`FILL` to be mixed with code directives arbitrarily. However all data is separated from code and placed sequentially in the data section. This behaviour is different from most other assemblers where data and code sections must be explicitly tagged.

