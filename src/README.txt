Please use the following commands to compile the system: 
Note: you must have LablGTK2 installed.

ocamllex lexer.mll
menhir parser.mly
ocamlc -c ast.mli
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamlc -c lexer.ml
ocamlc -c regex.mli
ocamlc -c regex.ml
ocamlc -c main.ml

ocamlfind ocamlc -g -package lablgtk2 -linkpkg parser.cmo lexer.cmo regex.cmo rsa.mli rsa.ml gui.mli gui.ml -o 2800-buddy

To Run: ./2800-buddy
