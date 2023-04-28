ocamlbuild -use-ocamlfind -use-menhir -package menhirLib -package unix -I src monkey.native
#ocamlopt -o bin/monkey -I src/ src/monkey.ml src/ast.ml src/lexer.ml
