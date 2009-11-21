include ../Makefile.config
all: libcudf.a c-test cudf.pc
include Makefile.variants

NULL =
CFLAGS = -Wall -DG_LOG_DOMAIN=\"libCUDF\"
PROG_CFLAGS = -Wall
INCLUDES = -I . $(shell pkg-config --cflags glib-2.0)
OCAML_LIBDIR := $(shell ocamlc -where)
CUDF_LDFLAGS = -L$(OCAML_LIBDIR) -L$(CURDIR)
CUDF_CLIBS = -lcudf -lm -ldl -lunix -lncurses -lglib-2.0
OCAMLC = ocamlfind ocamlc -package unix,extlib

libcudf.a: cudf-caml.o cudf.o
	cp $(OCAML_LIBDIR)/libcamlrun.a $@
	ar r $@ $^

../_build/%:
	cd .. && ocamlbuild $*

cudf-caml.o: ../_build/cudf.cma ../_build/cudf_c.cmo
	$(OCAMLC) -linkpkg -output-obj -o $@ $^

cudf.o: cudf.c cudf.h cudf-variants.h
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

c-test.o: c-test.c cudf.o
	$(CC) $(PROG_CFLAGS) $(INCLUDES) -c $<

c-test: c-test.o libcudf.a
	$(CC) $(PROG_CFLAGS) -o $@ $(INCLUDES) $(CUDF_LDFLAGS) $< $(CUDF_CLIBS)

test: ../tests/legacy.cudf ../tests/legacy-sol.cudf c-test
	./c-test ../tests/legacy.cudf ../tests/legacy-sol.cudf > /dev/null

cudf.pc: cudf.pc.in
	cat $< | sed -e "s,@OCAML_LIBDIR@,$(OCAML_LIBDIR),g" \
			-e "s,@VERSION@,$(VERSION),g" \
		> $@

clean:
	rm -f *.a *.o *.cmo *.cmi
	rm -f c-test
	rm -f cudf.pc

.PHONY: all clean test
