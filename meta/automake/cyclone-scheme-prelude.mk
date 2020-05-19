# cyclone-scheme.mk --
#
# This file must be included by "Makefile.am".

ACLOCAL_AMFLAGS		= -I meta/autotools
AUTOMAKE_OPTIONS	= foreign
EXTRA_DIST		= INSTALL
dist_doc_DATA		= README COPYING
CLEANFILES		=


#### building libraries

MMUX_CYCLONE_LIBS_PREREQ	=			\
	build-lib/config.scm				\
	build-lib/$(am__dirstamp)

CLEANFILES		+=				\
	build-lib/$(am__dirstamp)			\
	build-lib/*.sld					\
	build-lib/*.so					\
	build-lib/*.o					\
	build-lib/*.c					\
	build-lib/*.meta

CYCLONE_COMPILE_LIBS_ENV	= export LD_LIBRARY_PATH=$(builddir)/build-lib:$(CYCLONE_LIBRARY_DIRECTORY):$(LD_LIBRARY_PATH);
CYCLONE_COMPILE_LIBS_FLAGS	= -I $(CYCLONE_LIBRARY_DIRECTORY) -I $(builddir)/build-lib
CYCLONE_COMPILE_LIBS_LIB	= $(CYCLONE_COMPILE_LIBS_ENV) $(CYCLONE_COMPILER) $(CYCLONE_COMPILE_LIBS_FLAGS) $(CYCLONE_FLAGS)

CYCLONE_LIBRARY_DIRECTORY	= $(libdir)/cyclone

## --------------------------------------------------------------------

build-lib/$(am__dirstamp):
	@$(MKDIR_P) build-lib
	@: > build-lib/$(am__dirstamp)

build-lib/%.sld: lib/%.sld build-lib/$(am__dirstamp)
	cp $(<) $(@)


#### interface to "make check"
#
# Read "Parallel Test Harness" in the documentation of GNU Automake to
# understand how to use this interface for "make check".
#

CLEANFILES	+=				\
	build-tests/$(am__dirstamp)		\
	build-tests/*.scm			\
	build-tests/*.so			\
	build-tests/*.o				\
	build-tests/*.c				\
	build-tests/*.meta			\
	build-tests/*.exe

CYCLONE_COMPILE_CHECK_ENV	= export LD_LIBRARY_PATH=$(builddir)/build-lib:$(CYCLONE_LIBRARY_DIRECTORY):$(LD_LIBRARY_PATH);
CYCLONE_COMPILE_CHECK_FLAGS	= -I $(CYCLONE_LIBRARY_DIRECTORY) -I $(builddir)/build-lib
CYCLONE_COMPILE_CHECK_PROG	= $(CYCLONE_COMPILE_CHECK_ENV) $(CYCLONE_COMPILER) $(CYCLONE_COMPILE_CHECK_FLAGS) $(CYCLONE_FLAGS)

MMUX_CYCLONE_CHECK_PREREQ	=		\
	build-tests/$(am__dirstamp)

AM_TESTS_ENVIRONMENT		= export LD_LIBRARY_PATH=$(builddir)/build-lib:$(LD_LIBRARY_PATH);

## --------------------------------------------------------------------

build-tests/$(am__dirstamp):
	@$(MKDIR_P) build-tests
	@: > build-tests/$(am__dirstamp)


### end of file
