# Copyright (C) 2006 Free Software Foundation, Inc.
# This configure.ac script is free software; the Free Software Foundation
# gives unlimited permission to copy, distribute and modify it.

AC_PREREQ(2.59)
m4_include([version.m4])
AC_INIT([cascade],VERSION_NUMBER, [hannes.jung@desy.de])
AC_CONFIG_MACRO_DIRS([m4])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
## Checks for programs.
##AC_PROG_F77([g77 gfortran ])
##AC_FC_LIBRARY_LDFLAGS
# Check for programs 
# here is a trick to prefer 
#   gfortran for gcc4 and higher 
#   and g77 for gcc3 and lower 
flist="gfortran g77 f77" 
if [[[ "$(gcc -dumpversion | cut -d . -f 1)" < "4" ]]] ; then 
  flist="g77 f77 gfortran" 
fi
AC_PROG_F77([$flist ])
AC_FC_LIBRARY_LDFLAGS
AC_PROG_CXX g++
AC_PROG_CC g++
AM_PROG_AR

AC_PROG_INSTALL
AC_PROG_LN_S
AC_LIBTOOL_DLOPEN
AC_PROG_LIBTOOL

# zero all default fortran compiler flags
FFLAGS=""


# check for tools necessary to build documentation
#AC_PATH_PROG(DOXYGEN, doxygen, [no])
AC_PATH_PROG(LATEX, latex, [no])
AC_PATH_PROG(DVIPS, dvips, [no])
AC_PATH_PROG(PDFLATEX, pdflatex, [no])

# check if we have all the tools necessary to build documentation
AM_CONDITIONAL(BUILD_DOCS, test "x$build_docs" = "xyes" \
		             -a "x$DVIPS" != "xno" \
		             -a "x$LATEX" != "xno" )
# \
#		             -a "x$PDFLATEX" != "xno" )
# Checks for libraries.

AC_MSG_CHECKING([for GSL installation])
AC_ARG_WITH([gsl],[AS_HELP_STRING([--with-gsl],[path to GSL installation])],[:],[with_gsl=${GSL_DIR}])
AS_IF([test "x$with_gsl" != "xno" && [ test "x`ls $with_gsl/lib/libgsl*`" != "x" || test "x`ls $with_gsl/lib64/libgsl*`" != "x" ] ], [AC_SUBST(GSL_DIR,[$with_gsl])],
      AC_MSG_ERROR([Cannot find $with_gsl/lib -lgsl -lgslcblas libraries.
      Specify correct path to GSL installation: --with-gsl=/path/to/gsl]) ; exit 1)
GSL_CFLAGS=`${GSL_DIR}/bin/gsl-config --cflags`
AC_SUBST(GSL_CFLAGS)
#AC_MSG_RESULT([Using gsl include ${GSL_DIR}/bin/gsl-config --cflags ])
AC_MSG_RESULT([Using gsl include $GSL_DIR])
AC_SUBST([GSL_LIB],["-L$with_gsl/lib -L$with_gsl/lib64 -lgsl -lgslcblas"])
AC_MSG_RESULT([Using $GSL_LIB ])


AC_MSG_CHECKING([for BOOST installation])
AC_ARG_WITH([boost],[AS_HELP_STRING([--with-boost],[path to BOOST installation])],[:],[with_boost=${BOOST_CFLAGS}])
AS_IF([test "x$with_boost" != "xno" && test "x`ls $with_boost/boost/shared_ptr.*`" != "x" ], [AC_SUBST(BOOST_CFLAGS,[$with_boost])],
      AC_MSG_ERROR([Cannot find $with_boost/ includes.
      Specify correct path to BOOST installation: --with-boost=/path/to/boost]) ; exit 1)
AC_MSG_RESULT([Using $BOOST_CFLAGS ])


AC_MSG_CHECKING([for Pythia 6 libraries])
AC_ARG_WITH([pythia6],[AS_HELP_STRING([--with-pythia6],[path to Pythia6 installation])],[:],[with_pythia6=${PYTHIA6_DIR}])        
AS_IF([test "x$with_pythia6" != "xno" && [ test "x`ls $with_pythia6/lib/libpythia6.*`" != "x" || test "x`ls $with_pythia6/lib64/libpythia6.*`" != "x"] ], [AC_SUBST(PYTHIA6_DIR,[$with_pythia6])],
      AC_MSG_ERROR([Cannot find $with_pythia6/lib/libpythia6.* or $with_pythia6/lib64/libpythia6.* libraries. 
      Specify correct path to Pythia 6 installation: --with-pythia6=/path/to/pythia6]) ; exit 1)
AC_SUBST([PYTHIA_SOURCE], ["$with_pythia6/lib"])
AC_SUBST([PYTHIALIB], ["-L$with_pythia6/lib -L$with_pythia6/lib64 -lpythia6 -lpythia6_dummy "])
AC_MSG_RESULT([Using -L$with_pythia6/lib -L$with_pythia6/lib64 -lpythia6 -lpythia6_dummy  ])
 
    
AC_MSG_CHECKING([for Pythia8])
AC_ARG_WITH([pythia8],[AS_HELP_STRING([--with-pythia8],[path to Pythia8 installation])],[:],[with_pythia8=${PYTHIA8_DIR}])
AS_IF([test "x$with_pythia8" != "xno" && [ test "x`ls $with_pythia8/lib/libpythia8.*`" != "x"|| test "x`ls $with_pythia8/lib64/libpythia8.*`" != "x"] ], [AC_SUBST(PYTHIA8_DIR,[$with_pythia8])],
        AC_MSG_RESULT([Cannot find $with_pythia8/lib/libpythia8.* or $with_pythia8/lib64/libpythia8.* libraries. 
        Specify correct path to Pythia8 installation: --with-pythia8=/path/to/pythia8]) ; AC_MSG_RESULT([Building CASCADE without Pythia8]); AC_SUBST([no_pythia8], [1]))
if test x$no_pythia8 == x; then     
  AC_SUBST([PYTHIA8LIB], ["-L$with_pythia8/lib -L$with_pythia8/lib64 -lpythia8 "])
  AC_MSG_RESULT([Using $PYTHIA8LIB ])
fi  
AM_CONDITIONAL([NO_PYTHIA8],test $no_pythia8)     

AC_MSG_CHECKING([for LHAPDF])
AC_ARG_WITH([lhapdf],[AS_HELP_STRING([--with-lhapdf],[path to LHAPDF installation])],[:],[with_lhapdf=${LHAPDF_DIR}])
AS_IF([test "x$with_lhapdf" != "xno" && [ test "x`ls $with_lhapdf/lib/libLHAPDF.*`" != "x"||  test "x`ls $with_lhapdf/lib64/libLHAPDF.*`" != "x" ] ], [AC_SUBST(LHAPDF_DIR,[$with_lhapdf])],
      AC_MSG_ERROR([Cannot find $with_lhapdf/lib/libLHAPDF.* or $with_lhapdf/lib64/libLHAPDF.* libraries.
      Specify correct path to LHAPDF installation: --with-lhapdf=/path/to/lhapdf]) ; exit 1)
  AC_SUBST([PDFLIB], ["-L$with_lhapdf/lib -L$with_lhapdf/lib64 -lLHAPDF "])
  AC_MSG_RESULT([Using $PDFLIB ])


AC_MSG_CHECKING([for TMDlib])
AC_ARG_WITH([tmdlib],[AS_HELP_STRING([--with-tmdlib],[path to TMDlib installation])],[:],[with_tmdlib=${TMDLIB_DIR}])
AS_IF([test "x$with_tmdlib" != "xno" && [ test "x`ls $with_tmdlib/lib64/libTMDlib.*`" != "x" || test "x`ls $with_tmdlib/lib/libTMDlib.*`" != "x" ] ], [AC_SUBST(TMDLIB_DIR,[$with_tmdlib])],
        AC_MSG_RESULT([Cannot find $with_tmdlib/lib/libTMDlib.* or  $with_tmdlib/lib64/libTMDlib.* libraries. 
        Specify correct path to TMDlib installation: --with-tmdlib=/path/to/tmdlib]) ; AC_MSG_RESULT([Building CASCADE without TMDlib]); AC_SUBST([no_tmdlib], [1]))
if test x$no_tmdlib == x; then     
  AC_SUBST([TMDLIB], ["-L$with_tmdlib/lib -L$with_tmdlib/lib64 -lTMDlib "])
  AC_MSG_RESULT([Using $TMDLIB ])
fi  
AM_CONDITIONAL([NO_TMDLIB],test $no_tmdlib)     


AC_MSG_CHECKING([for HEPMC installation])
AC_ARG_WITH([hepmc],
   [AS_HELP_STRING([--with-hepmc], [path to HEPMC installation])],[:],[with_hepmc=${HEPMC}])
AS_IF([test "x$with_hepmc" != "xno" && [ test "x`ls $with_hepmc/lib/libHepMC.*`" != "x" ||  test "x`ls $with_hepmc/lib64/libHepMC.*`" != "x"] ], [AC_SUBST(HEPMC,[$with_hepmc])],
        AC_MSG_RESULT([Cannot find $with_hepmc/lib/libHepMC.* or $with_hepmc/lib64/libHepMC.* libraries. 
        Specify correct path to HEPMC installation: --with-hepmc=/path/to/hepmc]) ; AC_MSG_RESULT([Building RAPGAP without HepMC]); AC_SUBST([no_hepmc], [1]))
if test x$no_hepmc == x; then     
  AC_SUBST([HEPMCLIB], ["-L$with_hepmc/lib -L$with_hepmc/lib64 -lHepMC -lHepMCfio"])
  AC_MSG_RESULT([Building RAPGAP with HepMC: $HEPMCLIB])
fi  
AM_CONDITIONAL([NO_HEPMC],test $no_hepmc)     
     
if test "x$F77" = "xg77"; then
  AC_SUBST([MYFFLAGS], ["-fno-automatic -finit-local-zero -fno-backslash -fno-globals -ff90 -fpic -ffixed-line-length-132"])
  AC_MSG_RESULT([Using g77 with FLAGS = $MYFFLAGS ])
else
#  AC_SUBST([MYFFLAGS], ["-fno-automatic -fno-backslash -fpic"]) 
#  AC_SUBST([MYFFLAGS], ["-O0 -fbounds-check -fno-automatic -fno-backslash -fpic -ffixed-line-length-132"]) 
  AC_SUBST([MYFFLAGS], ["-O0 -std=legacy -fno-automatic -fno-backslash -fpic -ffixed-line-length-132"]) 
#  AC_SUBST([MYFFLAGS], ["-O0 -fno-automatic -fno-backslash -ffixed-line-length-132"]) 
#  AC_SUBST([MYFFLAGS], ["-O0 -fno-backslash -ffixed-line-length-132"]) 
#  AC_SUBST([MYFFLAGS], ["-O0 -std=legacy -ffixed-line-length-132"]) 
  AC_MSG_RESULT([Using gfortran with FLAGS = $MYFFLAGS ])
fi


if test "x$PDFLIB" = "x"; then
  AC_SUBST([PDFLIBSOURCE], ["pdfset.F   structm.F   structp.F"])
  AC_MSG_RESULT([using $PDFLIBSOURCE ])
else
  AC_SUBST([PDFLIBDSOURCE], [""])
  AC_MSG_RESULT([using pdfset.F   structm.F   structp.F from $lhapdf ])
fi


AC_SUBST(VERSION)

  AC_CONFIG_FILES([
  Makefile
   include/Makefile
   misc/Makefile
   bases51/Makefile
   src/Makefile
   data/Makefile
   manual/Makefile 
])

AC_OUTPUT
