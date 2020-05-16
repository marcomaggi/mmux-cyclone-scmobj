# @configure_input@
#

AC_PREREQ([2.69])
MMUX_PKG_VERSIONS([0],[1],[0],[devel.1])
AC_INIT([MMUX Cyclone Scmobj],[MMUX_PACKAGE_VERSION],
  [mrc.mgg@gmail.com],
  [mmux-cyclone-scmobj],[http://github.com/marcomaggi/mmux-cyclone-scmobj])
MMUX_INIT
AC_REVISION([0.1])
AC_COPYRIGHT([Copyright (c) 2020 Marco Maggi <mrc.mgg@gmail.com>

This program is free software: you  can redistribute it and/or modify it
under the terms of the GNU Lesser General Public License as published by
the Free  Software Foundation, either version  3 of the License,  or (at
your option) any later version.

This program  is distributed  in the  hope that it  will be  useful, but
WITHOUT   ANY   WARRANTY;  without   even   the   implied  warranty   of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
])
AC_CONFIG_SRCDIR([lib/])
AC_CONFIG_MACRO_DIR([meta/autotools])
AC_CONFIG_AUX_DIR([meta/autotools])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([1.16 foreign subdir-objects])
AM_MAINTAINER_MODE

AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_MKDIR_P


#### basic system inspection

MMUX_CHECK_TARGET_OS
AX_IS_RELEASE([git-directory])

AC_CACHE_SAVE


#### external libraries

MMUX_CYCLONE_SCHEME

AC_CACHE_SAVE


#### finish

AC_CONFIG_FILES([Makefile]
   [build-lib/config.scm:lib/config.scm.in])
MMUX_OUTPUT
AC_OUTPUT

### end of file
