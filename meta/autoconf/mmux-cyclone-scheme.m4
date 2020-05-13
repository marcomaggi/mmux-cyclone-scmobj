dnl mmux-cyclone-scheme.m4 --
dnl
dnl Finds Cyclone Scheme with compiler executable installed as "csc".

# Synopsis:
#
#    MMUX_CYCLONE_SCHEME
#
# Check  the  file  pathanmes  of the  CYCLONE  compiler  "cyclone"  and
# interpreter  "icyc";  set them,  respectively,  to  the variables  and
# substitutions:
#
#    CYCLONE_COMPILER
#    CYCLONE_INTERPRETER
#
# Acquire the release number of cyclone and store it in the variable and
# substitution:
#
#    CYCLONE_RELEASE_NUMBER
#
# then split  it into its components  and store them into  the variables
# and substitutions:
#
#    CYCLONE_RELEASE_MAJOR
#    CYCLONE_RELEASE_MINOR
#    CYCLONE_RELEASE_PATCH
#
AC_DEFUN([MMUX_CYCLONE_SCHEME],
  [AX_REQUIRE_DEFINED([MMUX_SPLIT_VERSION])

   AC_PATH_PROG([CYCLONE_COMPILER],[cyclone],[:])
   AC_PATH_PROG([CYCLONE_INTERPRETER],[icyc],[:])

   AC_CACHE_CHECK([CYCLONE release number],
     [mmux_cv_cyclone_release_NUMBER],
     [AS_VAR_SET(mmux_cv_cyclone_release_NUMBER,[$("$CYCLONE_INTERPRETER" -vn)])])
   AS_VAR_SET(CYCLONE_RELEASE_NUMBER,[$mmux_cv_cyclone_release_NUMBER])
   AC_SUBST([CYCLONE_RELEASE_NUMBER],[$CYCLONE_RELEASE_NUMBER])
   MMUX_SPLIT_VERSION([CYCLONE release],[mmux_cv_cyclone_release],[$mmux_cv_cyclone_release_NUMBER])
   AS_VAR_SET(CYCLONE_RELEASE_MAJOR, [$mmux_cv_cyclone_release_MAJOR_VERSION])
   AS_VAR_SET(CYCLONE_RELEASE_MINOR, [$mmux_cv_cyclone_release_MINOR_VERSION])
   AS_VAR_SET(CYCLONE_RELEASE_PATCH, [$mmux_cv_cyclone_release_PATCH_VERSION])
   AC_SUBST([CYCLONE_RELEASE_MAJOR],[$CYCLONE_RELEASE_MAJOR])
   AC_SUBST([CYCLONE_RELEASE_MINOR],[$CYCLONE_RELEASE_MINOR])
   AC_SUBST([CYCLONE_RELEASE_PATCH],[$CYCLONE_RELEASE_PATCH])

   # Flags variable  available on the  command line of  "configure" and
   # "make".
   #
   AS_VAR_SET_IF(CYCLONE_FLAGS,,[AS_VAR_SET(CYCLONE_FLAGS)])
   AC_SUBST([CYCLONE_FLAGS])
   ])

dnl end of file
dnl Local Variables:
dnl mode: autoconf
dnl End:
