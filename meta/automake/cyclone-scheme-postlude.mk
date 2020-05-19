# cyclone-scheme.mk --
#
# This file must be included by "Makefile.am".


#### documentation

EXTRA_DIST		+= doc/mmux-cyclone-docs.style.css
CLEANFILES		+= doc/mmux-cyclone-docs.css

AM_MAKEINFOFLAGS	= --no-split
AM_MAKEINFOHTMLFLAGS	= --split=node -c WORDS_IN_PAGE=0 --css-ref=mmux-cyclone-docs.css \
	-c PRE_BODY_CLOSE="<p>This document describes version <tt>$(PACKAGE_VERSION)</tt> of <em>$(PACKAGE_NAME)</em>.</p>"

## --------------------------------------------------------------------

doc/$(MMUX_CYCLONE_DOCSTEM).html/$(am__dirstamp): doc/$(am__dirstamp)
	@$(MKDIR_P) doc/$(MMUX_CYCLONE_DOCSTEM).html/
	@: > doc/$(MMUX_CYCLONE_DOCSTEM).html/$(am__dirstamp)

doc/$(MMUX_CYCLONE_DOCSTEM).html/mmux-cyclone-docs.css:		\
		doc/mmux-cyclone-docs.style.css			\
		doc/$(MMUX_CYCLONE_DOCSTEM).html/$(am__dirstamp)
	$(INSTALL) -m 0444 \
		"$(top_srcdir)/doc/mmux-cyclone-docs.style.css" \
		"$(builddir)/doc/$(MMUX_CYCLONE_DOCSTEM).html/mmux-cyclone-docs.css"

## --------------------------------------------------------------------

html-local: doc/$(MMUX_CYCLONE_DOCSTEM).html/mmux-cyclone-docs.css

install-html-local:
	$(MKDIR_P) "$(DESTDIR)$(htmldir)/$(MMUX_CYCLONE_DOCSTEM).html/"
	$(INSTALL) -m 0444 \
		$(builddir)/doc/$(MMUX_CYCLONE_DOCSTEM).html/mmux-cyclone-docs.css \
		"$(DESTDIR)$(htmldir)/$(MMUX_CYCLONE_DOCSTEM).html/"


#### building libraries

EXTRA_DIST		+= $(MMUX_CYCLONE_LIBS_SRCS)

.PHONY: libraries

libraries: $(MMUX_CYCLONE_LIBS_LIBS)

$(MMUX_CYCLONE_LIBS_METAS) $(MMUX_CYCLONE_LIBS_OBJS): $(MMUX_CYCLONE_LIBS_LIBS)

## --------------------------------------------------------------------

sharedlibdir		= $(CYCLONE_LIBRARY_DIRECTORY)
sharedlib_SCRIPTS	= $(MMUX_CYCLONE_LIBS_LIBS)

reqfilesdir		= $(CYCLONE_LIBRARY_DIRECTORY)
reqfiles_DATA		=		\
	$(MMUX_CYCLONE_LIBS_SRCS)	\
	$(MMUX_CYCLONE_LIBS_METAS)	\
	$(MMUX_CYCLONE_LIBS_OBJS)


#### interface to "make check"
#
# Read "Parallel Test Harness" in the documentation of GNU Automake to
# understand how to use this interface for "make check".
#

EXTRA_DIST	+= $(MMUX_CYCLONE_CHECK_SRCS)
TESTS		= $(MMUX_CYCLONE_CHECK_PROGS)

## --------------------------------------------------------------------

build-tests/%.scm: tests/%.scm $(MMUX_CYCLONE_CHECK_PREREQ)
	cp $(<) $(@)

build-tests/%: build-tests/%.scm
	$(CYCLONE_COMPILE_CHECK_PROG) $(<)

build-tests/%.exe: build-tests/%
	mv $(<) $(@)

## --------------------------------------------------------------------

.PHONY: test

test:
	for f in build-tests/*$(file)*.exe ; do $(AM_TESTS_ENVIRONMENT) $$f ; done


#### installcheck rule

MMUX_CYCLONE_INSTALLCHECK_ENV	=
MMUX_CYCLONE_INSTALLCHECK_ENV	+= LD_LIBRARY_PATH=$(DESTDIR)$(libdir):$(LD_LIBRARY_PATH); export LD_LIBRARY_PATH;
MMUX_CYCLONE_INSTALLCHECK_ENV	+= DYLD_LIBRARY_PATH=$(DESTDIR)$(libdir):$(DYLD_LIBRARY_PATH); export DYLD_LIBRARY_PATH;

installcheck-local:
	for f in $(TESTS) ; do $(MMUX_CYCLONE_INSTALLCHECK_ENV) $$f ; done

### end of file
