# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
m4_include([version.m4])
AC_INIT([HZTool],[4.3.2],[hannes.jung@desy.de],[hztool])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([config])
AM_INIT_AUTOMAKE
AC_LANG(C++)

## Library version flags (update with each new release version)
VERSIONFLAGS="-version-info 4:3:2" ## for 4.3.0
AC_SUBST(VERSIONFLAGS)

AC_CONFIG_SRCDIR([inc/bos.inc])
AC_CONFIG_HEADERS([config/config.h])

## Set Fortran compiler behaviour
if test "x$FCFLAGS" == "x"; then 
  FCFLAGS="-O2"
fi
## Try to respect users' Fortran compiler variables
if test "x$FC" == "x"; then 
  if test "x$F77" == "x"; then 
    FC="$GFORTRAN"
  else
    FC="$F77"
  fi
fi
AC_PROG_FC
AC_FC_LIBRARY_LDFLAGS

## Hack to make libtool use the FC Fortran compiler via F77, since
## libtool < 2.x doesn't understand --tag=FC
F77=$FC
#AM_FFLAGS=$AM_FCFLAGS
#FFLAGS="$FFLAGS $FCFLAGS"
FFLAGS=""
AC_SUBST(FCFLAGS)
AC_SUBST(FFLAGS)
AC_SUBST(F77)
AC_SUBST(AM_FFLAGS)

## Checks for programs
AC_PROG_FC
AC_PROG_LIBTOOL
AC_PROG_INSTALL
AC_PROG_LN_S

if test "x$MYFFLAGS" = "x"; then
if test "x$F77" = "xg77"; then
  AC_SUBST([MYFFLAGS], ["-fno-automatic -finit-local-zero -fno-backslash -fno-globals -ff90 -fpic -ffixed-line-length-132"])
  AC_MSG_RESULT([Using g77 with FLAGS = $MYFFLAGS ])
else
#  AC_SUBST([MYFFLAGS], ["-fno-automatic -fno-backslash -fpic"]) 
#  AC_SUBST([MYFFLAGS], ["-O0 -fbounds-check -fno-automatic -fno-backslash -fpic -ffixed-line-length-132"]) 
  AC_SUBST([MYFFLAGS], ["-O0 -fno-automatic -fno-backslash -fbounds-check -fpic -ffixed-line-length-132 -Wl,-flat_namespace "]) 
  AC_MSG_RESULT([Using gfortran with FLAGS = $MYFFLAGS ])
fi
else
AC_SUBST([MYFFLAGS], [$MYFFLAGS]) 
fi



##### check if we want to have secial routines for multiplcity in HERWIG and PYTHA
AM_CONDITIONAL([SPECIAL_OPT], [test "$special" = yes])
AC_MSG_RESULT([SPECIAL-OPTION: checking whether to include special = $special ])

##### check if we want to have secial routines for multiplcity in HERWIG and PYTHA
AC_MSG_CHECKING([checking for RIVET interface ])
AC_MSG_RESULT([RIVET-OPTION: checking whether to include rivet = $rivet ])

AM_CONDITIONAL([RIVET],test "$rivet" = yes)     

##########################

# check for documentation build flag
AC_ARG_ENABLE(docs,
    [  --disable-docs      disable documentation build],
    build_docs=$enableval, build_docs="yes")

# check for tools necessary to build documentation
#AC_PATH_PROG(DOXYGEN, doxygen, [no])
AC_PATH_PROG(LATEX, latex, [no])
AC_PATH_PROG(DVIPS, dvips, [no])
AC_PATH_PROG(PDFLATEX, pdflatex, [no])

# check if we have all the tools necessary to build documentation
AM_CONDITIONAL(BUILD_DOCS, test "x$build_docs" == "xyes" \
		             -a "x$DVIPS" != "xno" \
		             -a "x$LATEX" != "xno" )
# \
#		             -a "x$PDFLATEX" != "xno" )





##########################

AC_SUBST(VERSION)

AC_CONFIG_FILES([Makefile src/Makefile doc/Makefile doc/manual/Makefile inc/Makefile paw/Makefile])
AC_OUTPUT
