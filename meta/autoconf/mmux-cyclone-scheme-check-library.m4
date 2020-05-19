dnl mmux-cyclone-scheme-check-library.m4 --
dnl
dnl Checks if a Cyclone Scheme library is available on the system.

dnl Set the shell variable "cyclone_scheme_cv_schemelib_$1" to "yes" or "no".
dnl
dnl 1 OUTPUT_VARIABLE_COMPONENT_NAME
dnl 2 LIBRARY_IMPORT_SPEC
dnl 3 ACTION_IF_FOUND
dnl 4 ACTION_IF_NOT_FOUND
dnl
dnl Usage example:
dnl
dnl   MMUX_CYCLONE_SCHEME_CHECK_LIBRARY([MMUX_CYCLONE_CHECKS],[mmux.cyclone.checks],,)
dnl   AS_IF([test "$cyclone_scheme_cv_schemelib_MMUX_CYCLONE_CHECKS" = no],
dnl     [AC_MSG_ERROR([MMUX Cyclone Checks not available],1)])
dnl
AC_DEFUN([MMUX_CYCLONE_SCHEME_CHECK_LIBRARY],
  [AC_REQUIRE([MMUX_CYCLONE_SCHEME])
   AC_CACHE_CHECK([availability of Cyclone Scheme library $2],
     [cyclone_scheme_cv_schemelib_$1],
     [AS_IF(["$CYCLONE_INTERPRETER" -p '(import ($2))' ],
        [AS_VAR_SET([cyclone_scheme_cv_schemelib_$1],[yes])],
        [AS_VAR_SET([cyclone_scheme_cv_schemelib_$1],[no])])])
   AS_IF([test "$cyclone_scheme_cv_schemelib_MMUX_CYCLONE_CHECKS" = yes],$3,$4)])

dnl end of file
dnl Local Variables:
dnl mode: autoconf
dnl End:
