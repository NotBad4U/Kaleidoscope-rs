# Kaleidoscope-rs

Implementation of a simple programming frontend language using LLVM and Rust.
This exemple is a translation in Rust of the official tutorials available the LLVM official website:

Implementing a Language with LLVM in

* [C++](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html)
* [ocaml](https://llvm.org/docs/tutorial/OCamlLangImpl1.html)

To experiment with the code in this repo you need:

* the latest Rust compiler

* LLVM >= 9.0

To build the code just clone the repo and execute

```
cargo build
```

## The project overview

This simple language provides a JIT compilation or if you execute it without arguments,
it starts a REPL with an LLVM interpreter as execution engine.

* An DFA-based lexer with [plex](https://github.com/goffrie/plex) (`lexer.rs`)

*  LALR(1) parser with [plex](https://github.com/goffrie/plex). It works a bit like `yacc`. (`parser.rs`)

* the IR builder (`ircodegen.rs`)

* the JIT compiler, REPL, PassManager (`main.rs`)

## The language

This experimental language keep things simple, so the only datatype is a `64-bit` floating point type.
As such, all values are implicitly double precision and the language doesn’t require type declarations.
This gives the language a very nice and simple syntax.
For example, the following simple example computes *Factorial* numbers:

```
extern printd(x)

def sub(x,y)
    x - y

def facto(v)
    if v == 0 then
        1
   else
        v * facto(sub(v, 1))

main do printd( facto(10) )
```

## The grammar

This grammar description uses the dialect of *Extended Backus-Naur Form* (EBNF):

```
program          : [[statement | expression] Delimiter ? ]*;
statement        : [declaration | definition];
declaration      : Extern prototype;
definition       : Def prototype expression;
prototype        : Ident OpeningParenthesis [Ident Comma ?]* ClosingParenthesis;
expression       : [primary_expr (Op primary_expr)*];
primary_expr     : [Ident | Number | call_expr | parenthesis_expr];
call_expr        : Ident OpeningParenthesis [expression Comma ?]* ClosingParenthesis;
parenthesis_expr : OpeningParenthesis expression ClosingParenthesis;
```

