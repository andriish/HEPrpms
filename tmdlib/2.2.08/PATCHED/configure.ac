
AC_PREREQ([2.65])
m4_include([version.m4])
AC_CONFIG_MACRO_DIR([m4])

AC_INIT([TMDlib],[TMDLIB_VERSION_NUMBER],[hannes.jung@desy.de])
AM_INIT_AUTOMAKE
flist="gfortran g77 f77" 
if [[[ "$(gcc -dumpversion | cut -d . -f 1)" < "4" ]]] ; then 
  flist="g77 f77 gfortran" 
fi
AC_PROG_F77([$flist ])
AC_PROG_CXX([g++]) 
AC_FC_LIBRARY_LDFLAGS
AC_F77_LIBRARY_LDFLAGS
AM_PROG_AR

AC_PROG_INSTALL
AC_PROG_LN_S
_LT_SET_OPTION([LT_INIT],[dlopen])
m4_warn([obsolete],[AC_LIBTOOL_DLOPEN: Remove this warning and the call to _LT_SET_OPTION when you
put the 'dlopen' option into LT_INITs first parameter.])

LT_INIT


# Checks for libraries.
# checks for compiler flags     
if test "x$F77" = "xg77"; then
  AC_SUBST([MYFFLAGS], ["-fno-automatic -finit-local-zero -fno-backslash -fno-globals -ff90 -fpic -ffixed-line-length-132"])
  AC_MSG_RESULT([using g77 with FLAGS = $MYFFLAGS ])
else
  AC_SUBST([MYFFLAGS], ["-O0 -fno-backslash -fpic -ffixed-line-length-132"]) 
  AC_MSG_RESULT([using gfortran with FLAGS = $MYFFLAGS ])
fi

AC_MSG_CHECKING([for root installation])
root_config=`which root-config`
if test x$root_config == x; then
  AC_MSG_RESULT([Using $root_config])
  AC_MSG_WARN([Unable to find root-config.])
  AC_MSG_WARN([Building TMDlib without ROOT])
#  ROOT_CFLAGS='-stdlib=libc++ -std=c++11'
  AC_SUBST(ROOT_CFLAGS)
else 
  AC_MSG_RESULT([Using $root_config])
  root_ok=1
  ROOT_CFLAGS=`root-config --cflags`
  ROOT_LDFLAGS=`root-config --ldflags`
  ROOT_LIBS=`root-config --libs`
  AC_SUBST(ROOT_CFLAGS)
  AC_SUBST(ROOT_LDFLAGS)
  AC_SUBST(ROOT_LIBS)
fi

AM_CONDITIONAL([HAVE_ROOT],test $root_ok)


AC_MSG_CHECKING([for LHAPDF])

AC_ARG_WITH([lhapdf],
   [AS_HELP_STRING([--with-lhapdf], [path to LHAPDF installation])],[:],[with_lhapdf=${LHAPDF_DIR}])
AS_IF([test "x$with_lhapdf" != "xno" && [ test "x`ls $with_lhapdf/lib/libLHAPDF.*`" != "x"||  test "x`ls $with_lhapdf/lib64/libLHAPDF.*`" != "x" ] ], [AC_SUBST(LHAPDF_DIR,[$with_lhapdf])],
        AC_MSG_ERROR([Cannot find $with_lhapdf/lib/libLHAPDF.* or $with_lhapdf/lib64/libLHAPDF.* libraries.
        Specify correct path to LHAPDF installation: --with-lhapdf=/path/to/lhapdf]) ; exit 1
        LHAPDF_CFLAGS=`lhapdf-config --cflags`)
AC_SUBST([PDFLIB], ["-L$with_lhapdf/lib -L$with_lhapdf/lib64 -lLHAPDF "])
LHAPDF_CFLAGS=`${LHAPDF_DIR}/bin/lhapdf-config --cppflags`
AC_SUBST(LHAPDF_CFLAGS)
AC_MSG_RESULT([Using $PDFLIB ])

AC_SUBST(AM_CXXFLAGS)


CONFIG_LIBS=""
AC_SUBST(CONFIG_LIBS)


AC_MSG_CHECKING([for gsl installation])
gsl_config=`which gsl-config`
if test x$gsl_config == x; then
  AC_MSG_RESULT([Using $gsl_config])
  AC_MSG_ERROR([Cannot find $gsl_config.
     Specify correct path to GSL installation: which gsl-config]) 
else 
  AC_MSG_RESULT([Using $gsl_config])
  GSL_CFLAGS=`gsl-config --cflags`
  GSL_LIB=`gsl-config --libs`
  AC_SUBST(GSL_CFLAGS)
  AC_SUBST(GSL_LIB)
fi



# set the default destination directory
AC_PREFIX_DEFAULT(/usr/local)

# TMDlib configuration script
AC_CONFIG_FILES([TMDlib-config:TMDlib-config.in], [chmod u+x TMDlib-config])
# TMDlib tool for grid files script
AC_CONFIG_FILES([TMDlib-getdata:TMDlib-getdata.in], [chmod u+x TMDlib-getdata])


AC_SUBST(VERSION)

  AC_CONFIG_FILES([
  Makefile
  include/Makefile
  src/Makefile
  apfelxx/inc/Makefile
  apfelxx/src/Makefile
  data/Makefile
  examples-c++/Makefile
  examples-fortran/Makefile
  TMDplotter/Makefile
  manual/Makefile
])

AC_OUTPUT
