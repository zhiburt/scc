# scc(simple-c-compiler)

A compiler of C code to assembly. The great part of which is done with consideration to [this artical compilation](https://norasandler.com/2017/11/29/Write-a-Compiler.html)

It uses `gasm` sytaxis for assembly files.

> _This project takes its origin from education aims._

## Usage

The compiler generates `asm.s` file by default.

```
cargo run main.c
```

There's a script which imitates the machure compiler to generate binary imidiately.
The command below will produce binary file.

```
bash compiler.sh main.c
```

## Current state

At present it works only with `int` type and does not have support of global variables and macros.

There are a lot of things that should be done by the book to craft it in appropriate shape.


### :soon: roadmap 

- Refactoring
- Enhancement in error handling
- Lifetime calculation
  - Allocation algorithm
  - Alignment callucation
- A bunch of optimizations (e.g remove unused code, calculate const expressions)
- Support `char*` type
- ? Structures
- ? Arrays
- ? Support more basic types such as `long`, `char` etc.
- ? support `arm` assembly?
- ? support `x32`
- ? support `NASM` syntax for `x32/64`
