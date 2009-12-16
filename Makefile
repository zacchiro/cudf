include Makefile.config

NAME = cudf

LIBS = _build/cudf.cma
LIBS_OPT = _build/cudf.cmxa
PROGS = _build/main_cudf_check _build/main_cudf_parse_822
PROGS_BYTE = $(addsuffix .byte,$(PROGS))
PROGS_OPT = $(addsuffix .native,$(PROGS))
RESULTS = $(LIBS) $(PROGS_BYTE) _build/cudf_c.cmo
RESULTS_OPT = $(LIBS_OPT) $(PROGS_OPT) _build/cudf_c.cmx
SOURCES = $(wildcard *.ml *.mli *.mll *.mly)
C_LIB_DIR = c-lib
C_LIB_SOURCES = $(wildcard $(C_LIB_DIR)/*.c $(C_LIB_DIR)/*.h)

OCAMLBUILD = ocamlbuild
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

DIST_DIR = $(NAME)-$(VERSION)
DIST_TARBALL = $(DIST_DIR).tar.gz
DEB_TARBALL = $(subst -,_,$(DIST_DIR).orig.tar.gz)

all: $(RESULTS)
opt: $(RESULTS_OPT)
$(RESULTS): $(SOURCES)
$(RESULTS_OPT): $(SOURCES)

.PHONY: c-lib
c-lib:
	make -C $(C_LIB_DIR)

clean:
	make -C $(C_LIB_DIR) clean
	$(OCAMLBUILD) $(OBFLAGS) -clean
	rm -rf *.dsc *.deb *.changes *.orig.tar.gz \
		$(NAME)-*.gz $(NAME)_*.gz $(NAME)-*/

_build/%:
	$(OCAMLBUILD) $(OBFLAGS) $*
	@touch $@

# top-level: _build/cudf.cma _build/tests.cmo
top-level: _build/cudf.cma
	ledit ocaml -I ./_build/ -init ./.ocamlinit-cudf

headers: header.txt .headache.conf
	headache -h header.txt -c .headache.conf $(SOURCES) $(C_LIB_SOURCES)

test: _build/test.byte
	$< -verbose
c-lib-test:
	make -C $(C_LIB_DIR) test
_build/test.byte: $(SOURCES)

tags: TAGS
TAGS: $(SOURCES)
	otags $^

INSTALL_STUFF = META
INSTALL_STUFF += $(wildcard _build/*.cma _build/*.cmxa _build/cudf.a)
INSTALL_STUFF += $(wildcard _build/cudf_*.cmi) $(wildcard _build/*.mli)
INSTALL_STUFF += $(wildcard _build/cudf_*.cmx _build/cudf_*.o _build/cudf_*.a)
INSTALL_STUFF += $(wildcard _build/cudf.o _build/cudf.cmx _build/cudf.cmi)

install:
	test -d $(LIBDIR) || mkdir -p $(LIBDIR)
	$(INSTALL) -patch-version $(VERSION) $(NAME) $(INSTALL_STUFF)
	test -d $(BINDIR) || mkdir -p $(BINDIR)
	for p in $(notdir $(PROGS)) ; do \
		tgt=`echo $$p | sed -e 's/^main.//' -e 's/_/-/g'` ; \
		if [ -f _build/$$p.native ] ; then \
			cp _build/$$p.native $(BINDIR)/$$tgt ; \
		else \
			cp _build/$$p.byte $(BINDIR)/$$tgt ; \
		fi ; \
		echo "Installed $(BINDIR)/$$tgt" ; \
	done

uninstall:
	$(UNINSTALL) $(NAME)
	for p in $(notdir $(PROGS)) ; do \
		tgt=`echo $$p | sed -e 's/^main.//' -e 's/_/-/g'` ; \
		if [ -f $(BINDIR)/$$tgt ] ; then \
			rm $(BINDIR)/$$tgt ; \
		fi ; \
		echo "Removed $(BINDIR)/$$tgt" ; \
	done
	-rmdir -p $(LIBDIR) $(BINDIR)

dist: ./$(DIST_TARBALL)
./$(DIST_TARBALL):
	if [ -d ./$(DIST_DIR)/ ] ; then rm -rf ./$(DIST_DIR)/ ; fi
	if [ -d ./$(DIST_TARBALL) ] ; then rm -f ./$(DIST_TARBALL) ; fi
	svn export . ./$(DIST_DIR)
	rm -rf ./$(DIST_DIR)/debian
	tar cvzf ./$(DIST_TARBALL) ./$(DIST_DIR)
	rm -rf ./$(DIST_DIR)
	@echo "Distribution tarball: ./$(DIST_TARBALL)"

./$(DEB_TARBALL): ./$(DIST_TARBALL)
	cp $< $@
deb: ./$(DEB_TARBALL)
	rm -rf ./$(DIST_DIR)
	tar xvzf $<
	svn export debian/ $(DIST_DIR)/debian
	cd $(DIST_DIR) && dpkg-buildpackage -rfakeroot

distcheck: ./$(DIST_TARBALL)
	tar xzf $<
	$(MAKE) -C ./$(DIST_DIR) all
	if which ocamlopt > /dev/null ; then $(MAKE) -C ./$(DIST_DIR) opt ; fi
	$(MAKE) -C ./$(DIST_DIR) test
	$(MAKE) -C ./$(DIST_DIR)/$(C_LIB_DIR)/ all
	$(MAKE) -C ./$(DIST_DIR) install DESTDIR=$(CURDIR)/$(DIST_DIR)/tmp
	rm -rf ./$(DIST_DIR)

doc:
	$(OCAMLBUILD) $(OBFLAGS) cudf.docdir/index.html

.PHONY: all opt clean top-level headers test tags install uninstall dist doc
