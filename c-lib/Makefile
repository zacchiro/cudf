CFLAGS = -I .
LDFLAGS = \
	-L$(shell ocamlc -where) \
	-L$(shell ocamlfind query pcre)
CLIBS = -lpcre -lm -ldl -lpcre_stubs -lunix -lcamlrun -lncurses
OCAMLC = ocamlfind ocamlc -package unix,extlib,pcre

all: cudf.o cudf-caml.o test

cudf-caml.o: ../_build/cudf.cma ../_build/cudf_c.cmo
	$(OCAMLC) -linkpkg -output-obj -o $@ $^

cudf.o: cudf.c cudf.h
	$(CC) $(CFLAGS) -c $<

test.o: test.c
	$(CC) $(CFLAGS) -c $<

test: cudf-caml.o cudf.o test.o
	$(CC) -o $@ $(CFLAGS) $(LDFLAGS) $^ $(CLIBS)

caml_hash_variant: caml_hash_variant.o
	> dummy.ml
	ocamlc -o dummy.o -output-obj dummy.ml
	$(CC) -o $@ $< dummy.o -L"$(shell ocamlc -where)" -lcamlrun -lm -lcurses
	@rm -f dummy.*

clean:
	rm -f cudf.o

.PHONY: all clean
