LIBS = _build/cudf.cma
PROGS = _build/cudf-check.byte
RESULTS = $(LIBS) $(PROGS)
SOURCES = $(wildcard *.ml *.mli)
OCAMLBUILD = ocamlbuild
# OBFLAGS = -classic-display
OBFLAGS =

all: $(RESULTS)
$(RESULTS): $(SOURCES)

clean:
	$(OCAMLBUILD) $(OBFLAGS) -clean

_build/%:
	$(OCAMLBUILD) $(OBFLAGS) $*
	@touch $@

top-level: _build/cudf.cma
	ledit ocaml -I ./_build/ -init ./.ocamlinit-cudf

headers: header.txt .headache.conf
	headache -h header.txt -c .headache.conf $(SOURCES)

test: _build/test.byte
	$<
_build/test.byte: $(SOURCES)

.PHONY: all clean top-level headers test
