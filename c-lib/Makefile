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
	$(CC) $(CFLAGS) $(LDFLAGS) $^ $(CLIBS)


clean:
	rm -f cudf.o

.PHONY: all clean
