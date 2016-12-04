make:
	ocamlbuild -pkgs oUnit main.byte

clean:
	ocamlbuild -use-ocamlfind -clean

test:
	ocamlbuild -use-menhir -pkgs oUnit test.byte && ./test.byte