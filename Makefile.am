## Process this file with automake to produce Makefile.in

include meta/automake/cyclone-scheme-prelude.mk


#### documentation

MMUX_CYCLONE_DOCSTEM			= mmux-cyclone-scmobj

info_TEXINFOS				= doc/mmux-cyclone-scmobj.texi
doc_mmux_cyclone_scmobj_TEXINFOS	= 	\
	doc/macros.texi				\
	doc/lgpl-3.0.texi			\
	doc/fdl-1.3.texi


#### library build targets

MMUX_CYCLONE_LIBS_SRCS		=			\
	lib/mmux.cyclone.scmobj.version.sld		\
	lib/mmux.cyclone.scmobj.helpers.sld		\
	lib/mmux.cyclone.scmobj.sld

MMUX_CYCLONE_LIBS_LIBS		=			\
	build-lib/mmux.cyclone.scmobj.version.so	\
	build-lib/mmux.cyclone.scmobj.helpers.so	\
	build-lib/mmux.cyclone.scmobj.so

MMUX_CYCLONE_LIBS_METAS		=			\
	build-lib/mmux.cyclone.scmobj.version.meta	\
	build-lib/mmux.cyclone.scmobj.helpers.meta	\
	build-lib/mmux.cyclone.scmobj.meta

MMUX_CYCLONE_LIBS_OBJS		=			\
	build-lib/mmux.cyclone.scmobj.version.o		\
	build-lib/mmux.cyclone.scmobj.helpers.o		\
	build-lib/mmux.cyclone.scmobj.o

## --------------------------------------------------------------------

build-lib/mmux.cyclone.scmobj.version.so: 			\
		build-lib/mmux.cyclone.scmobj.version.sld	\
		$(MMUX_CYCLONE_LIBS_PREREQ)
	$(CYCLONE_COMPILE_LIBS_LIB) $(<)

build-lib/mmux.cyclone.scmobj.helpers.so: 			\
		build-lib/mmux.cyclone.scmobj.helpers.sld	\
		$(MMUX_CYCLONE_LIBS_PREREQ)
	$(CYCLONE_COMPILE_LIBS_LIB) $(<)

build-lib/mmux.cyclone.scmobj.so: 				\
		build-lib/mmux.cyclone.scmobj.sld		\
		build-lib/mmux.cyclone.scmobj.helpers.so	\
		build-lib/mmux.cyclone.scmobj.version.so	\
		$(MMUX_CYCLONE_LIBS_PREREQ)
	$(CYCLONE_COMPILE_LIBS_LIB) $(<)


#### interface to "make check"
#
# Read "Parallel Test Harness" in the documentation of GNU Automake to
# understand how to use this interface for "make check".
#

MMUX_CYCLONE_CHECK_SRCS		=		\
	tests/test-version.scm			\
	tests/test-scmobj.scm

MMUX_CYCLONE_CHECK_PROGS	=		\
	build-tests/test-version.exe		\
	build-tests/test-scmobj.exe


#### common stuff

include meta/automake/cyclone-scheme-postlude.mk

### end of file
