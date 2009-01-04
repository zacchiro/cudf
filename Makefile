LIBS = _build/cudf.cma
PROGS = _build/cudf-check.byte
RESULTS = $(LIBS) $(PROGS)
SOURCES = $(wildcard *.ml *.mli)
all: $(RESULTS)
$(RESULTS): $(SOURCES)

clean:
	ocamlbuild -clean

_build/%:
	ocamlbuild $*
	@touch $@

top-level: _build/cudf.cma
	ledit ocaml -I ./_build/ -init ./.ocamlinit-cudf

headers: header.txt .headache.conf
	headache -h header.txt -c .headache.conf $(SOURCES)

.PHONY: all clean top-level headers
