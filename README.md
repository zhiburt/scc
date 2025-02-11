# scc

It is a compiler for a subset of C programing language. \
The target platform is `x86_64`.

It is self relying - has no dependencies (almost).

## Build

```
cargo install --path .
```

## Usage

```man
Usage: scc [OPTIONS] <INPUT_FILE>

Arguments:
  <INPUT_FILE>  The input file, written in C programming language

Options:
  -l, --pretty-lex             Prints tokens which are produced by lexical analyzer to stdout
  -a, --pretty-ast             Prints AST which are produced by syntax analyse stage to stdout
  -t, --pretty-tac             Prints IR(Three Address Code) to stdout
  -O                           Activate optimizations
  -s, --syntax <[intel|gasm]>  Assembly syntax of the output file
  -o <OUT_FILE>                The output file, in which will be carried out a compilation
  -h, --help                   Print help
  -V, --version                Print version
```

`scc` generates an assembly but for use as a major compiler, you can use a next script.

```
./compiler.sh main.c
```

## Example

Code:

```c
int fib(int n) {
    if (n == 0 || n == 1) {
        return n;
    } else {
        return fib(n - 1) + fib(n - 2);
    }
}

int main() {
    int n = 6;
    return fib(n);
}
```

LEX:

```text
[Int, Identifier, OpenParenthesis, Int, Identifier, CloseParenthesis, OpenBrace, If, OpenParenthesis, Identifier, Equal, IntegerLiteral, Or, Identifier, Equal, IntegerLiteral, CloseParenthesis, OpenBrace, Return, Identifier, Semicolon, CloseBrace, Else, OpenBrace, Return, Identifier, OpenParenthesis, Identifier, Negation, IntegerLiteral, CloseParenthesis, Addition, Identifier, OpenParenthesis, Identifier, Negation, IntegerLiteral, CloseParenthesis, Semicolon, CloseBrace, CloseBrace, Int, Identifier, OpenParenthesis, CloseParenthesis, OpenBrace, Int, Identifier, Assignment, IntegerLiteral, Semicolon, Return, Identifier, OpenParenthesis, Identifier, CloseParenthesis, Semicolon, CloseBrace]
```

AST:

```text
FUNCTION fib:
  parameters: INT n
  body:
IF VAR[n] BIN_OP<Equal> Int(0) BIN_OP<Or> VAR[n] BIN_OP<Equal> Int(1):
  RETURN VAR[n]
ELSE:
  RETURN CALL fib WITH VAR[n] BIN_OP<Sub> Int(1) BIN_OP<Addition> CALL fib WITH VAR[n] BIN_OP<Sub> Int(2)
END

FUNCTION main:
  parameters: 
  body:
INT n = Int(6)
RETURN CALL fib WITH VAR[n]
```

IR:

```text
_fib:
  param n
  BeginFunc 36
  t1: 0
  t2: n == 0
  t3: 1
  IfZ t2 Goto _L1
  Goto _L3
_L1:
  t4: n == 1
  IfZ t4 Goto _L2
  Goto _L3
_L2:
  t3: 0
_L3:
  IfZ t3 Goto _L4
  t1: n
  Goto _L0
  Goto _L5
_L4:
  t5: n - 1
  PushParam t5
  t6: LCall _fib
  PopParams 4
  t7: n - 2
  PushParam t7
  t8: LCall _fib
  PopParams 4
  t9: t6 + t8
  t1: t9
  Goto _L0
_L5:
_L0:
  Return t1

intervals fib
{0: Range { start: 1, end: 19 }, 1: Range { start: 0, end: 26 }, 2: Range { start: 1, end: 3 }, 3: Range { start: 2, end: 12 }, 4: Range { start: 6, end: 7 }, 5: Range { start: 17, end: 18 }, 6: Range { start: 18, end: 21 }, 7: Range { start: 19, end: 20 }, 8: Range { start: 20, end: 21 }, 9: Range { start: 21, end: 22 }}


main:
  BeginFunc 8
  n: 6
  PushParam n
  t11: LCall _fib
  PopParams 4
  Return t11

intervals main
{10: Range { start: 0, end: 1 }, 11: Range { start: 1, end: 2 }}
```

ASM:

```asm
  .globl fib
  .text
fib:
    pushq %rbp
    movq %rsp, %rbp
    subq $36, %rsp
    movl %edi, -4(%rbp)
    movl $0, -16(%rbp)
    cmpl $0, -4(%rbp)
    sete %dil
    andb $1, %dil
    movzbl %dil, %edi
    movl %edi, -8(%rbp)
    movl $1, -12(%rbp)
    cmpl $0, -8(%rbp)
    je _L1
    jmp _L3
_L1:
    cmpl $1, -4(%rbp)
    sete %dil
    andb $1, %dil
    movzbl %dil, %edi
    movl %edi, %edi
    cmpl $0, %edi
    je _L2
    jmp _L3
_L2:
    movl $0, -12(%rbp)
_L3:
    cmpl $0, -12(%rbp)
    je _L4
    movl -4(%rbp), %edi
    movl %edi, -16(%rbp)
    jmp _L0
    jmp _L5
_L4:
    movl -4(%rbp), %edi
    movl %edi, -20(%rbp)
    subl $1, -20(%rbp)
    movl -20(%rbp), %edi
    call fib
    movl %eax, -24(%rbp)
    movl -4(%rbp), %edi
    movl %edi, -28(%rbp)
    subl $2, -28(%rbp)
    movl -28(%rbp), %edi
    call fib
    movl %eax, -32(%rbp)
    movl -24(%rbp), %edi
    movl %edi, -36(%rbp)
    movl -32(%rbp), %edi
    addl %edi, -36(%rbp)
    movl -36(%rbp), %edi
    movl %edi, -16(%rbp)
    jmp _L0
_L5:
_L0:
    movl -16(%rbp), %eax
    addq $36, %rsp
    movq %rbp, %rsp
    popq %rbp
    ret

  .globl main
  .text
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $0, %rsp
    movl $6, %edx
    movl %edx, %edi
    call fib
    movl %eax, %ecx
    movl %ecx, %eax
    addq $0, %rsp
    movq %rbp, %rsp
    popq %rbp
    ret
```

## :negative_squared_cross_mark: Not supported yet

- More basic types `long`, `short`, `char`, etc.
- `struct`ures, `union`s, `enum`s
- arrays
- pointers
- macros
- preprocessor is not run.

## References

- A good list of articles how to create a compiler step by step. The whole process is laid on naturally in a methodical way. — https://norasandler.com/2017/11/29/Write-a-Compiler.html
- Openly available slides of cs143 standford compilers class. It had an influence on a allocation stage. — https://web.stanford.edu/class/archive/cs/cs143/cs143.1128/
- Some notes about IR — https://cs.lmu.edu/~ray/notes/ir/