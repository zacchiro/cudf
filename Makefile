all: _build/cudf.cma _build/cudf-check.byte

clean:
	ocamlbuild -clean

_build/%:
	ocamlbuild $*

top-level: _build/cudf.cma
	ledit ocaml -I ./_build/ -init ./.ocamlinit-cudf
