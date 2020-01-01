# scc(simple-c-compiler)

A compiler of C code to assembly. The great part of which is done with consideration to [Nora's articals](https://norasandler.com/2017/11/29/Write-a-Compiler.html)

It uses `gasm` sytaxis for assembly files.

## Usage

As we more interested in binary file exists a small bash script which generate assembly code and after that by gcc generate binary file. This script as straightforward as it can be, it generates assembly file with the same name as input file has only with sufix __.s__

This script does not clean the assembly file

```
compiler.sh file1
```


## Current state

Nowadays it works only with integer types. Without support of global variables (it might be managed in the nearest).
The assembly code is kind of freaky in the look :)

There are a lot of things that should be done by the book to get it in healthy shape. The most important ones is presented in the issues tab.

> This project takes it's origin from education aims.
