# Copyright (C) 2006 Free Software Foundation, Inc.
# This configure.ac script is free software; the Free Software Foundation
# gives unlimited permission to copy, distribute and modify it.

AC_PREREQ(2.59)
m4_include([version.m4])
AC_INIT([rapgap],VERSION_NUMBER, [hannes.jung@desy.de])
AC_CONFIG_MACRO_DIRS([m4])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
## Checks for programs.

flist="gfortran g77 f77" 
if [[[ "$(gcc -dumpversion | cut -d . -f 1)" < "4" ]]] ; then 
  flist="g77 f77 gfortran" 
fi
AC_PROG_F77([$flist ])
AC_FC_LIBRARY_LDFLAGS
AC_PROG_CXX g++
AM_PROG_AR

AC_PROG_INSTALL
AC_PROG_LN_S
AC_LIBTOOL_DLOPEN
AC_PROG_LIBTOOL



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


AC_MSG_CHECKING([for Pythia 6 libraries])
AC_ARG_WITH([pythia6],[AS_HELP_STRING([--with-pythia6],[path to Pythia6 installation])],[:],[with_pythia6=${PYTHIA6_DIR}])        
AS_IF([test "x$with_pythia6" != "xno" && [ test "x`ls $with_pythia6/lib/libpythia6.*`" != "x" || test "x`ls $with_pythia6/lib64/libpythia6.*`" != "x"] ], [AC_SUBST(PYTHIA6_DIR,[$with_pythia6])],
      AC_MSG_ERROR([Cannot find $with_pythia6/lib/libpythia6.* or $with_pythia6/lib64/libpythia6.* libraries. 
      Specify correct path to Pythia 6 installation: --with-pythia6=/path/to/pythia6]) ; exit 1)
AC_SUBST([PYTHIA_SOURCE], ["$with_pythia6/lib"])
AC_SUBST([PYTHIALIB], ["-L$with_pythia6/lib -L$with_pythia6/lib64 -lpythia6 -lpythia6_dummy "])
AC_MSG_RESULT([Using -L$with_pythia6/lib -L$with_pythia6/lib64 -lpythia6 -lpythia6_dummy  ])
 
 

AC_MSG_CHECKING([for LHAPDF])
AC_ARG_WITH([lhapdf],[AS_HELP_STRING([--with-lhapdf],[path to LHAPDF installation])],[:],[with_lhapdf=${LHAPDF_DIR}])
AS_IF([test "x$with_lhapdf" != "xno" && [ test "x`ls $with_lhapdf/lib/libLHAPDF.*`" != "x"||  test "x`ls $with_lhapdf/lib64/libLHAPDF.*`" != "x" ] ], [AC_SUBST(LHAPDF_DIR,[$with_lhapdf])],
      AC_MSG_ERROR([Cannot find $with_lhapdf/lib/libLHAPDF.* or $with_lhapdf/lib64/libLHAPDF.* libraries.
      Specify correct path to LHAPDF installation: --with-lhapdf=/path/to/lhapdf]) ; exit 1)
  AC_SUBST([PDFLIB], ["-L$with_lhapdf/lib -L$with_lhapdf/lib64 -lLHAPDF "])
  AC_MSG_RESULT([Using $PDFLIB ])


AC_MSG_CHECKING([for HEPMC installation])
AC_ARG_WITH([hepmc],
   [AS_HELP_STRING([--with-hepmc], [path to HEPMC installation])],[:],[with_hepmc=${HEPMC}])
AS_IF([test "x$with_hepmc" != "xno" && [ test "x`ls $with_hepmc/lib/libHepMC.*`" != "x" ||  test "x`ls $with_hepmc/lib64/libHepMC.*`" != "x"||  test "x`ls $with_hepmc/lib/x86_64-linux-gnu/libHepMC.*`" != "x"] ], [AC_SUBST(HEPMC,[$with_hepmc])],
        AC_MSG_RESULT([Cannot find $with_hepmc/lib/libHepMC.* or $with_hepmc/lib64/libHepMC.* libraries. 
        Specify correct path to HEPMC installation: --with-hepmc=/path/to/hepmc]) ; AC_MSG_RESULT([Building RAPGAP without HepMC]); AC_SUBST([no_hepmc], [1]))
if test x$no_hepmc == x; then     
  AC_SUBST([HEPMCLIB], ["-L$with_hepmc/lib -L$with_hepmc/lib64 -L$with_hepmc/lib/x86_64-linux-gnu -lHepMC -lHepMCfio"])
  AC_MSG_RESULT([Building RAPGAP with HepMC: $HEPMCLIB])
fi  
AM_CONDITIONAL([NO_HEPMC],test $no_hepmc)     


AC_MSG_CHECKING([for HEPMC3 installation])
AC_ARG_WITH([hepmc3],
   [AS_HELP_STRING([--with-hepmc3], [path to HEPMC3 installation])],[:],[with_hepmc3=${HEPMC3}])
AS_IF([test "x$with_hepmc3" != "xno" && [ test "x`ls $with_hepmc3/lib/libHepMC3.*`" != "x" ||  test "x`ls $with_hepmc3/lib64/libHepMC3.*`" != "x"] ], [AC_SUBST(HEPMC3,[$with_hepmc3])],
        AC_MSG_RESULT([Cannot find $with_hepmc3/lib/libHepMC3.* or $with_hepmc3/lib64/libHepMC3.* libraries. 
        Specify correct path to HEPMC3 installation: --with-hepmc3=/path/to/hepmc3]) ; AC_MSG_RESULT([Building RAPGAP without HepMC3]); AC_SUBST([no_hepmc3], [1]))
if test x$no_hepmc3 == x; then     
  AC_SUBST([HEPMC3LIB], ["-L$with_hepmc3/lib -L$with_hepmc3/lib64 -lHepMC3"])
  CXXFLAGS="${CXXFLAGS} -std=c++1y"
  AC_MSG_RESULT([Building RAPGAP with HepMC3: $HEPMC3LIB])


AS_IF([[ test "x`ls $with_hepmc3/lib/root/libHepMC3rootIO.*`" != "x" ||  test "x`ls $with_hepmc3/lib64/root/libHepMC3rootIO.*`" != "x" ||  test "x`ls $with_hepmc3/lib/libHepMC3rootIO.*`" != "x" ||  test "x`ls $with_hepmc3/lib64/libHepMC3rootIO.*`" != "x" ] ], 
       [AC_SUBST(HEPMC3,[$with_hepmc3])],
        AC_MSG_RESULT([Cannot find $with_hepmc3/lib/root/libHepMC3rootIO.* or $with_hepmc3/lib64/root/libHepMC3rootIO.* or $with_hepmc3/lib/libHepMC3rootIO.* or $with_hepmc3/lib64/libHepMC3rootIO.* libraries. 
        Specify correct path to HEPMC3 installation: --with-hepmc3=/path/to/hepmc3 and make sure the rootIO libraries are installed]) ; 
        AC_MSG_RESULT([Building RAPGAP without HepMC3rootIO]); AC_SUBST([no_hepmc3rootIO], [1]))
if test x$no_hepmc3rootIO == x; then     
  AC_SUBST([HEPMC3LIBROOTIO], [" -L$with_hepmc3/lib -L$with_hepmc3/lib64 -L$with_hepmc3/root/lib -L$with_hepmc3/lib64/root -lHepMC3 -lHepMC3rootIO"])
  CXXFLAGS="${CXXFLAGS} -std=c++1y -DHEPMC3_ROOTIO=1 "
  AC_MSG_RESULT([Building RAPGAP with HepMC3rootIO: $HEPMC3LIBROOTIO])

fi  
fi  
AM_CONDITIONAL([NO_HEPMC3],test $no_hepmc3)     



AC_MSG_CHECKING([for Rivet installation])
AC_ARG_WITH([rivet],
   [AS_HELP_STRING([--with-rivet], [path to RIVET installation])],[:],[with_rivet=${HEPMC}])
AS_IF([test "x$with_rivet" != "xno" && [ test "x`ls $with_rivet/lib/libRivet.*`" != "x" ||  test "x`ls $with_rivet/lib64/libRivet.*`" != "x"] ], [AC_SUBST(RIVET,[$with_rivet])],
        AC_MSG_RESULT([Cannot find $with_rivet/lib/libRivet.* or $with_rivet/lib64/libRivet.* libraries. 
        Specify correct path to RIVET installation: --with-rivet=/path/to/rivet]) ; AC_MSG_RESULT([Building RAPGAP without Rivet]); AC_SUBST([no_rivet], [1]))
if test x$no_rivet == x; then     
  AC_SUBST([RIVETLIB], ["-L$with_rivet/lib -L$with_rivet/lib64 -lRivet"])
  CXXFLAGS="${CXXFLAGS} -std=c++1y -g"
  AC_MSG_RESULT([Building RAPGAP with Rivet: $RIVETLIB])
fi  
AM_CONDITIONAL([NO_RIVET],test $no_rivet)     

AM_CONDITIONAL([HZTOOL_OPT], [test "$hzexe" = yes])
   
if test "x$F77" = "xg77"; then
  AC_SUBST([MYFFLAGS], ["-g -fno-automatic -finit-local-zero -fno-backslash -fno-globals -ff90 -fpic -ffixed-line-length-132"])
  AC_MSG_RESULT([using g77 with FLAGS = $MYFFLAGS ])
else
#  AC_SUBST([MYFFLAGS], ["-fno-automatic -fno-backslash -fpic"]) 
  AC_SUBST([MYFFLAGS], ["-g -O0 -fbounds-check -fno-automatic -fno-backslash -fpic -ffixed-line-length-132"]) 
#  AC_SUBST([MYFFLAGS], ["-O0 -fno-automatic -fno-backslash -fpic -ffixed-line-length-132"]) 
  AC_MSG_RESULT([using gfortran with FLAGS = $MYFFLAGS ])
fi


if test "x$PDFLIB" = "x"; then
  AC_SUBST([PDFLIBSOURCE], ["pdfset.F   structm.F   structp.F"])
  AC_MSG_RESULT([using $PDFLIBSOURCE ])
else
  AC_SUBST([PDFLIBDSOURCE], [""])
  AC_MSG_RESULT([using pdfset.F   structm.F   structp.F from $lhapdf ])
fi


AC_SUBST(VERSION)

AM_CONDITIONAL([ARI_OPT], [test "$ariadne" = yes])
if test "x$ariadne" != "x"; then
  AC_MSG_CHECKING([for ARIADNE library])
  if test "x$ARIADNE" = "x"; then
    AC_MSG_ERROR([ARIADNE environment variable is not set!
    ARIADNE must be set to something like: 
    ARIADNE="/Users/jung/jung/cvs/ariadne412"])
  fi
  if test "x$ARIADNE" != "x"; then
    AC_SUBST([ARIADNELIB], ["../ariadne/librgar4.la -L$ARIADNE/lib -L$ARIADNE/lib64 -lar4"])
    AC_MSG_RESULT([Using ../ariadne/librgar4.la -L$ARIADNE/lib -lar4  ])
  fi
fi
if test "x$ariadne" = "x"; then
    AC_SUBST([ARIADNELIB], ["../ariadne-dummy/libar4.la"])
    AC_MSG_RESULT([Using ../ariadne-dummy/libar4.la  ])
fi

AC_CONFIG_FILES([
 Makefile
 include/Makefile
 misc/Makefile
 bases51/Makefile
 ariadne/Makefile
 ariadne-dummy/Makefile
 src/Makefile
 examples/Makefile
 data/Makefile
 manual/Makefile 
])

AC_OUTPUT
