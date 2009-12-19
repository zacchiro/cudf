include ../Makefile.config
all: libcudf.a c-test cudf.pc
opt: libcudf-opt.a c-test-opt cudf.pc
include Makefile.variants

NULL =
CFLAGS = -Wall -DG_LOG_DOMAIN=\"libCUDF\"
PROG_CFLAGS = -Wall
INCLUDES = -I . -I $(OCAML_LIBDIR) $(shell pkg-config --cflags glib-2.0)
OCAML_LIBDIR := $(shell ocamlc -where)
CUDF_LDFLAGS = -L$(OCAML_LIBDIR) -L$(CURDIR)
CUDF_CLIBS = -lcudf -lm -ldl -lunix -lncurses -lglib-2.0
OCAMLC = ocamlfind ocamlc -package unix,extlib
OCAMLOPT = ocamlfind ocamlopt -package unix,extlib

INSTALL_STUFF = libcudf.a cudf.h cudf.pc

libcudf.a: cudf-caml.o cudf.o
	cp $(OCAML_LIBDIR)/libcamlrun.a $@
	ar r $@ $^
libcudf-opt.a: cudf-caml-opt.o cudf.o
	cp $(OCAML_LIBDIR)/libasmrun.a $@
	ar r $@ $^

../_build/%:
	cd .. && ocamlbuild $*

cudf-caml.o: ../_build/cudf.cma ../_build/cudf_c.cmo
	$(OCAMLC) -linkpkg -output-obj -o $@ $^
cudf-caml-opt.o: ../_build/cudf.cmxa ../_build/cudf_c.cmx
	$(OCAMLOPT) -linkpkg -output-obj -o $@ $^

cudf.o: cudf.c cudf.h cudf-variants.h

c-test.o: c-test.c cudf.o
	$(CC) $(PROG_CFLAGS) $(INCLUDES) -c $<

c-test: c-test.o libcudf.a
	$(CC) $(PROG_CFLAGS) -o $@ $(INCLUDES) $(CUDF_LDFLAGS) $< $(CUDF_CLIBS)
c-test-opt: c-test.o libcudf-opt.a
	$(CC) $(PROG_CFLAGS) -o $@ $(INCLUDES) $(CUDF_LDFLAGS) $< \
		$(subst -lcudf,-lcudf-opt,$(CUDF_CLIBS))

test: ../tests/legacy.cudf ../tests/legacy-sol.cudf c-test
	./c-test ../tests/legacy.cudf ../tests/legacy-sol.cudf > /dev/null

cudf.pc: cudf.pc.in
	cat $< | sed -e "s,@OCAML_LIBDIR@,$(OCAML_LIBDIR),g" \
			-e "s,@VERSION@,$(VERSION),g" \
		> $@

%.o: %.c
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

clean:
	rm -f *.a *.o *.cmo *.cmi
	rm -f c-test c-test-opt
	rm -f cudf.pc

install:
	test -d $(CLIBDIR) || mkdir -p $(CLIBDIR)
	test -d $(CINCDIR) || mkdir -p $(CINCDIR)
	test -d $(PCDIR) || mkdir -p $(PCDIR)
	if [ -f libcudf-opt.a ] ; then \
		cp libcudf-opt.a $(CLIBDIR)/libcudf.a ; \
	else \
		cp libcudf.a $(CLIBDIR)/libcudf.a ; \
	fi
	cp libcudf.a $(CLIBDIR)
	cp cudf.h $(CINCDIR)
	cp cudf.pc $(PCDIR)

uninstall:
	rm $(CLIBDIR)/libcudf.a
	rm $(CINCDIR)/cudf.h
	rm $(PCDIR)/cudf.pc
	-rmdir -p $(CLIBDIR) $(CINCDIR) $(PCDIR)

.PHONY: all opt clean test install uninstall
