include Makefile.config

NAME = cudf

LIBS = _build/cudf.cma
LIBS_OPT = _build/cudf.cmxa
PROGS = _build/cudf-check.byte
PROGS_OPT = _build/cudf-check.native
RESULTS = $(LIBS) $(PROGS) _build/cudf_c.cmo
RESULTS_OPT = $(LIBS_OPT) $(PROGS_OPT) _build/cudf_c.cmx
SOURCES = $(wildcard *.ml *.mli)
C_LIB_SOURCES = $(wildcard c-lib/*.c c-lib/*.h)

OCAMLBUILD = ocamlbuild
# OBFLAGS = -classic-display
OBFLAGS =
OCAMLFIND = ocamlfind

DESTDIR =
LIBDIR = $(DESTDIR)/$(shell ocamlc -where)
BINDIR = $(DESTDIR)/usr/bin
ifeq ($(DESTDIR),)
INSTALL = $(OCAMLFIND) install
UNINSTALL = $(OCAMLFIND) remove
else
INSTALL = $(OCAMLFIND) install -destdir $(LIBDIR)
UNINSTALL = $(OCAMLFIND) remove -destdir $(LIBDIR)
endif

all: $(RESULTS)
opt: $(RESULTS_OPT)
$(RESULTS): $(SOURCES)
$(RESULTS_OPT): $(SOURCES)

.PHONY: c-lib
c-lib:
	make -C c-lib/

clean:
	make -C c-lib/ clean
	$(OCAMLBUILD) $(OBFLAGS) -clean

_build/%:
	$(OCAMLBUILD) $(OBFLAGS) $*
	@touch $@

top-level: _build/cudf.cma _build/tests.cmo
	ledit ocaml -I ./_build/ -init ./.ocamlinit-cudf

headers: header.txt .headache.conf
	headache -h header.txt -c .headache.conf $(SOURCES) $(C_LIB_SOURCES)

test: _build/test.byte
	$<
_build/test.byte: $(SOURCES)

tags: TAGS
TAGS: $(SOURCES)
	otags $^

INSTALL_STUFF = META
INSTALL_STUFF += $(wildcard _build/*.cma _build/*.cmxa)
INSTALL_STUFF += $(wildcard _build/cudf_*.cmi) $(wildcard *.mli)
INSTALL_STUFF += $(wildcard _build/cudf_*.cmx _build/cudf_*.o _build/cudf_*.a)
INSTALL_STUFF += $(wildcard _build/cudf.o _build/cudf.cmx _build/cudf.cmi)

install:
	test -d $(LIBDIR) || mkdir -p $(LIBDIR)
	$(INSTALL) -patch-version $(VERSION) $(NAME) $(INSTALL_STUFF)
	test -d $(BINDIR) || mkdir -p $(BINDIR)
	if [ -f _build/cudf-check.native ] ; then \
		cp _build/cudf-check.native $(BINDIR)/cudf-check ; \
	else \
		cp _build/cudf-check.byte $(BINDIR)/cudf-check ; \
	fi
	@echo "Installed $(BINDIR)/cudf-check"

uninstall:
	$(UNINSTALL) $(NAME)
	if [ -f $(BINDIR)/cudf-check ] ; then \
		rm $(BINDIR)/cudf-check ; \
	fi
	@echo "Removed $(BINDIR)/cudf-check"

.PHONY: all clean top-level headers test tags
