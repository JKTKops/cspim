# cspim

A C-to-MIPS compiler targetting QtSpim. Inspired for CS233@UIUC's SpimBot competition, but the plan is to grow the project beyond that.

CSpim is not a fully featured C compiler. It may never be. Therefore it is not concerned with conforming
100% to the C standard but rather being a solid, optimizing compiler for a C-like language and/or an (improper) subset of C.

Unfortunately the assembling/linking story for QtSpim is complicated. Therefore CSpim does not do either.
There is a planned --create-make-dir flag which will output the generated assembly files along with copies of the
standard library assembly files into a directory. Ideally it will also produce a Makefile in that directory, such that
```bash
$ cspim main.c --create-make-dir -o main
$ cd main
$ make
```
will spawn a QtSpim instance that loads all of the generated files.

Planned features:

    [x] Variables
    [x] int type
    [ ] Integral (signed/unsigned int/short/char) types, but not long
    [x] return statements
    [ ] if, else, for, while, do-while control flow
    [ ] goto
    [ ] basic C expressions with operators
    [ ] arrays
    [ ] typedefs
    [ ] structs
 
These features are planned for completion in time for SpimBot.

These additional features are planned longer-term goals:

    [ ] Optimization engine
    [ ] Register allocator (+ tagging variables with 'live accross function call' so that temps can be saved)
    [ ] floating point types and operations
 
If CSpim becomes a powerful enough compiler, it may eventually be a target for ProtoHaskell so that I can say I've written an
end-to-end Haskell compiler :)
