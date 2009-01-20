include ../Makefile.config

NULL =
CFLAGS = -Wall
INCLUDES = -I . $(shell pkg-config --cflags glib-2.0)
OCAML_LIBDIR := $(shell ocamlc -where)
OCAML_PCRE_LIBDIR := $(shell ocamlfind query pcre)
CUDF_LDFLAGS = -L$(OCAML_LIBDIR) -L$(OCAML_PCRE_LIBDIR) -L$(CURDIR)
CUDF_CLIBS = -lcudf -lpcre -lm -ldl -lpcre_stubs -lunix -lncurses -lglib-2.0
OCAMLC = ocamlfind ocamlc -package unix,extlib,pcre

all: libcudf.a test cudf.pc

libcudf.a: cudf-caml.o cudf.o
	cp $(OCAML_LIBDIR)/libcamlrun.a $@
	ar r $@ $^

../_build/%:
	cd .. && ocamlbuild $*

cudf-caml.o: ../_build/cudf.cma ../_build/cudf_c.cmo
	$(OCAMLC) -linkpkg -output-obj -o $@ $^

cudf.o: cudf.c cudf.h
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

test.o: test.c
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

test: test.o libcudf.a
	$(CC) $(CFLAGS) -o $@ $(INCLUDES) $(CUDF_LDFLAGS) $< $(CUDF_CLIBS)

caml_hash_variant: caml_hash_variant.o
	> dummy.ml
	ocamlc -o dummy.o -output-obj dummy.ml
	$(CC) $(CFLAGS) -o $@ $< dummy.o -L$(OCAML_LIBDIR) -lcamlrun -lm -lcurses
	@rm -f dummy.*

cudf.pc: cudf.pc.in
	cat $< | sed -e "s,@OCAML_LIBDIR@,$(OCAML_LIBDIR),g" \
			-e "s,@OCAML_PCRE_LIBDIR@,$(OCAML_PCRE_LIBDIR),g" \
			-e "s,@VERSION@,$(VERSION),g" \
		> $@

clean:
	rm -f *.a *.o *.cmo *.cmi
	rm -f caml_hash_variant test
	rm -f dummy.*
	rm -f cudf.pc

.PHONY: all clean
