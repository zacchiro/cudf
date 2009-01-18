CFLAGS = -I .
LDFLAGS = -L$(shell ocamlc -where) -lunix -lcamlrun -lpcre
OCAMLC = ocamlfind ocamlc -package unix,extlib,pcre

all: cudf.o cudf-caml.o test

cudf-caml.o: ../_build/cudf.cma ../_build/cudf_c.cmo
	$(OCAMLC) -linkpkg -output-obj -o $@ $^

cudf.o: cudf.c cudf.h
	$(CC) $(CFLAGS) -c $<

test.o: test.c
	$(CC) $(CFLAGS) $<

test: cudf-caml.o cudf.o
	$(CC) $(CFLAGS) $(LDFLAGS) $^


clean:
	rm -f cudf.o

.PHONY: all clean
