.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean

zip:
	zip -r ocamtris.zip . -x _build/\* .git/\*
<<<<<<< HEAD
=======

docs:
	dune build @doc
>>>>>>> 1e71e9687a42fe0d6c17221788abb37bdd6d3bf6
