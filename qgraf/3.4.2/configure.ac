
AC_INIT([qgraf], [3.1], [andrii.verbytskyi@mpp.mpg.de], [qgraf],  [subdir-objects])
AC_PREREQ([2.59])

AM_INIT_AUTOMAKE([1.9.6 tar-pax -Wall  no-define])

AC_PREFIX_DEFAULT ([`pwd`])

AC_LANG([C++])
AC_PROG_CXX

AC_LANG([C])
AC_PROG_CC
AM_PROG_CC_C_O

AC_PROG_F77([ifort xlf pgfortran gfortran])

m4_ifdef([AM_PROG_AR], [AM_PROG_AR])
AC_PROG_RANLIB


AC_SUBST([EXAMPLE_SO_VERSION], [0:0:0])
AC_SUBST([EXAMPLE_API_VERSION], [${PACKAGE_VERSION}])
AC_SUBST([PACKAGE_URL], [http://www.desy.de/~andriish/])

# Override the template file name of the generated .pc file, so that there
# is no need to rename the template file when the API version changes.
AC_CONFIG_FILES([Makefile])

AC_OUTPUT

