NULL =
CFLAGS = -I . $(shell pkg-config --cflags glib-2.0) -Wall
LDFLAGS = \
	-L$(shell ocamlc -where) \
	-L$(shell ocamlfind query pcre) \
	-L$(CURDIR) \
	$(NULL)
CLIBS = -lcudf -lpcre -lm -ldl -lpcre_stubs -lunix -lncurses -lglib-2.0
OCAMLC = ocamlfind ocamlc -package unix,extlib,pcre

all: libcudf.a test

libcudf.a: cudf-caml.o cudf.o
	cp "$(shell ocamlc -where)"/libcamlrun.a $@
	ar r $@ $^

../_build/%:
	cd .. && ocamlbuild $*

cudf-caml.o: ../_build/cudf.cma ../_build/cudf_c.cmo
	$(OCAMLC) -linkpkg -output-obj -o $@ $^

cudf.o: cudf.c cudf.h
	$(CC) $(CFLAGS) -c $<

test.o: test.c
	$(CC) $(CFLAGS) -c $<

test: test.o libcudf.a
	$(CC) -o $@ $(CFLAGS) $(LDFLAGS) $< $(CLIBS)

caml_hash_variant: caml_hash_variant.o
	> dummy.ml
	ocamlc -o dummy.o -output-obj dummy.ml
	$(CC) -o $@ $< dummy.o -L"$(shell ocamlc -where)" -lcamlrun -lm -lcurses
	@rm -f dummy.*

clean:
	rm -f *.a *.o *.cmo *.cmi
	rm -f caml_hash_variant test
	rm -f dummy.*

.PHONY: all clean
