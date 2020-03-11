# Done For Now

Now that CS233@UIUC's Fall 2019 SpimBot competition has ended (I won!), my interest in the project has been severely waning. Chief among several issues is that the rushed aspects of the project accrued quite some technical debt that is still hard to work with. And, extremely annoyingly, `TemplateHaskell` and Intero do not play together well, and this project uses significant amounts of `TemplateHaskell`. In the compiler's current state, it implements `goto` and `if` (but not `else`) as well as function definitions and calls, which is more than enough to support a Turing-complete subset of C. The project also has an engine for optimizing the output MIPS. Ideally, this engine would've been realized with a pattern-based DSL, but the tech debt of not parameterizing the `MipsInstruction` type with the type of registers it contains caught up to me. This refactor would be worth digging time into on return. When the ghcide + hie engine is ready, I may return to this project (assuming that it plays well with `TemplateHaskell`) to implement the remaining control flow and an optimization engine that works on the IR.

The old README can be seen below.

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

- [x] Variables
- [x] int type
- [x] Integral (signed/unsigned int/short/char) types, but not long
- [x] return statements
- [ ] if, else, for, while, do-while control flow
- [x] goto
- [x] basic C expressions with operators (in significant progress)
- [ ] arrays
- [ ] typedefs
- [ ] structs

These additional features are planned longer-term goals:

- [ ] Optimization engine
- [ ] Register allocator (+ tagging variables with 'live accross function call' so that temps can be saved)
- [ ] floating point types and operation
