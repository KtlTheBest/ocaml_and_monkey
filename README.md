# ocaml_and_monkey
My Monkey Programming Language Interpretator. We need more monkeys

## What is it?

Basically a [Monkey programming language](https://monkeylang.org/). I tried to keep it as accurate to the specification as possible, but I failed as soon as I decided to not to accept semicolons, and so I gave up.

## Features:
 - Lambdas
 - Closures
 - Recursion
 - Immutability(?)
 - Garbage collected (by OCaml)
 - Written in OCaml
 - Any bugs that you find
 
## How to build
Make sure you have opam and base compiler, a.k.a. OCaml 4.13 or 4.14 installed (I don't remember which one). Then, with opam install:
```
opam install ocamlfind
opam install ocamlbuild
opam install menhir
```

And after that run:
```
bash build.sh
```

When the compilation is successful, you can call the executable:
```
./monkey.native examples/example3.mnk
```

Have fun!
