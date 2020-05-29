# scc

It is a compiler for a subset of C programing language. The target platform is `x86_64`.

## Build

The command below produces a binary in `~/.cargo/bin`. Or you can build by `cargo build` which will produce a binary in `./target` folder.

```
cargo install --path .
```

## Usage

```man
USAGE:
    simple-c-compiler [FLAGS] [OPTIONS] <input-file>

ARGS:
    <input-file>    The input file, written in C programming language

FLAGS:
    -h, --help          Prints help information
    -O                  Activate optimizations
    -a, --pretty-ast    Prints AST which are produced by syntax analyse stage to stdout
    -l, --pretty-lex    Prints tokens which are produced by lexical analyzer to stdout
    -t, --pretty-tac    Prints IR(Three Address Code) to stdout
    -V, --version       Prints version information

OPTIONS:
    -o <out-file>                  The output file, in which will be carried out a compilation
    -s, --syntax <[intel|gasm]>    Assembly syntax of the output file
```

You can run it by `cargo run` or use the built binary.

There's a bash script which imitates a mature compiler which generates binary.
The command below will produce binary file.

```
bash compiler.sh main.c
```

## :negative_squared_cross_mark: Not supported yet

- macros
- basic types `long`, `short`, `char`, etc.
- structures
- arrays
- pointers
- `char*`

## References

- A good list of articles how to create a compiler step by step. The whole process is laid on naturally in a methodical way. — https://norasandler.com/2017/11/29/Write-a-Compiler.html
- Openly available slides of cs143 standford compilers class. It had an influence on a allocation stage. — https://web.stanford.edu/class/archive/cs/cs143/cs143.1128/
- Some notes about IR — https://cs.lmu.edu/~ray/notes/ir/