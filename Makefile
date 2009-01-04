LIBS = _build/cudf.cma
PROGS = _build/cudf-check.byte
RESULTS = $(LIBS) $(PROGS)
all: $(RESULTS)
$(RESULTS): $(wildcard *.ml *.mli)

clean:
	ocamlbuild -clean

_build/%:
	ocamlbuild $*
	@touch $@

top-level: _build/cudf.cma
	ledit ocaml -I ./_build/ -init ./.ocamlinit-cudf
