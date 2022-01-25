# Copyright (C) 2006 Free Software Foundation, Inc.
# This configure.ac script is free software; the Free Software Foundation
# gives unlimited permission to copy, distribute and modify it.

#AC_PREREQ([2.71])
m4_include([version.m4])
AC_INIT([rapgap],[VERSION_NUMBER],[hannes.jung@desy.de])
#AC_CONFIG_MACRO_DIRS([m4])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
## Checks for programs.


AC_PROG_CC([g++])
AC_PROG_FC([gfortran])
AC_PROG_F77([gfortran])
AC_PROG_CPP([g++])
AC_PROG_CXX([g++])
AM_PROG_AR
AC_FC_LIBRARY_LDFLAGS
AC_FC_WRAPPERS
AC_F77_WRAPPERS

#Use libtoolize to build libraries
LT_INIT
AC_CONFIG_MACRO_DIR([m4])

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
 
 

AC_MSG_CHECKING([for LHAPDF])
AC_ARG_WITH([lhapdf5],[AS_HELP_STRING([--with-lhapdf5],[path to LHAPDF installation])],[:],[with_lhapdf5=${LHAPDF_DIR}])
AC_ARG_WITH([lhapdf6],[AS_HELP_STRING([--with-lhapdf6],[path to LHAPDF installation])],[:],[with_lhapdf6=${LHAPDF_DIR}])

if test "x$with_lhapdf5" != "x"; then
	AC_SUBST(LHAPDF_DIR,[$with_lhapdf5]) 
      AC_SUBST([lhapdf5], [1])
	AC_SUBST([PDFLIB], ["-L$with_lhapdf5/lib -L$with_lhapdf5/lib64 -lLHAPDF "])
      AC_MSG_RESULT([Building RAPGAP with lhapdf5])
elif test "x$with_lhapdf6" != "x"; then
	AC_SUBST(LHAPDF_DIR,[$with_lhapdf6]) 
      AC_SUBST([lhapdf5], [0])
	AC_SUBST([PDFLIB], ["-L$with_lhapdf6/lib -L$with_lhapdf6/lib64 -lLHAPDF "])
      AC_MSG_RESULT([Building RAPGAP with lhapdf6])
fi

AC_MSG_RESULT([Using $PDFLIB ])

# CHANGED by Kolja Kauder: Support non-standard library location
#AC_MSG_CHECKING([for LHAPDF])
#AC_ARG_WITH([lhapdf],[AS_HELP_STRING([--with-lhapdf],[path to LHAPDF installation])],[:],[with_lhapdf=${LHAPDF_DIR}])
#AC_SUBST([PDFLIBPATH],`find $with_lhapdf -name libLHAPDF.\* | head -n 1`)
#AS_IF( [test "x$PDFLIBPATH" == "x"] , [AC_MSG_ERROR([Cannot find libLHAPDF.* under $with_lhapdf
#      Specify correct path to LHAPDF installation: --with-lhapdf=/path/to/lhapdf] )], [AC_SUBST([PDFLIB], ["-L`dirname $PDFLIBPATH` -lLHAPDF #"])])
#AC_MSG_RESULT([Using $PDFLIB ])

AM_CONDITIONAL([LHAPDF5],test $lhapdf5)     
#AC_MSG_RESULT([Using lhapdf5 $LHAPDF5 ])


AC_MSG_CHECKING([for HEPMC2 installation])
AC_ARG_WITH([hepmc2],[AS_HELP_STRING([--with-hepmc2], [path to HEPMC2 installation])],[:],[with_hepmc2=${HEPMC2}])
   
AS_IF([test "x$vhepmc2" != "xno" && [ test "x`ls $with_hepmc2/lib/libHepMC.*`" != "x" ||  test "x`ls $with_hepmc2/lib64/libHepMC.*`" != "x"] ], [AC_SUBST(HEPMC2,[$with_hepmc2])],
        AC_MSG_RESULT([Cannot find $with_hepmc2/lib/libHepMC.* or $with_hepmc2/lib64/libHepMC.* libraries. 
        Specify correct path to HEPMC2 installation: --with-hepmc2=/path/to/hepmc2]) ; AC_MSG_RESULT([Building RAPGAP without HepMC2]); AC_SUBST([no_hepmc2], [1]))
if test x$no_hepmc2 == x; then     
  AC_SUBST([HEPMC2LIB], ["-L$with_hepmc2/lib -L$with_hepmc2/lib64 -lHepMC -lHepMCfio"])
  AC_MSG_RESULT([Building RAPGAP with HepMC2: $HEPMC2LIB])
fi  
AM_CONDITIONAL([NO_HEPMC2],test $no_hepmc2)     


AC_MSG_CHECKING([for HEPMC3 installation])
AC_ARG_WITH([hepmc3],
   [AS_HELP_STRING([--with-hepmc3], [path to HEPMC3 installation])],[:],[with_hepmc3=${HEPMC3}])
AS_IF([test "x$with_hepmc3" != "xno" && [ test "x`ls $with_hepmc3/lib/libHepMC3.*`" != "x" ||  test "x`ls $with_hepmc3/lib64/libHepMC3.*`" != "x"] ], [AC_SUBST(HEPMC3,[$with_hepmc3])],
        AC_MSG_RESULT([Cannot find $with_hepmc3/lib/libHepMC3.* or $with_hepmc3/lib64/libHepMC3.* libraries. 
        Specify correct path to HEPMC3 installation: --with-hepmc3=/path/to/hepmc3]) ; AC_MSG_RESULT([Building RAPGAP without HepMC3]); AC_SUBST([no_hepmc3], [1]))
if test x$no_hepmc3 == x; then     
  AC_SUBST([HEPMC3LIB], ["-L$with_hepmc3/lib -L$with_hepmc3/lib64 -lHepMC3"])
  CXXFLAGS="${CXXFLAGS}"
  AC_MSG_RESULT([Building RAPGAP with HepMC3: $HEPMC3LIB])


AS_IF([[ test "x`ls $with_hepmc3/lib/root/libHepMC3rootIO.*`" != "x" ||  test "x`ls $with_hepmc3/lib64/root/libHepMC3rootIO.*`" != "x" ||  test "x`ls $with_hepmc3/lib/libHepMC3rootIO.*`" != "x" ||  test "x`ls $with_hepmc3/lib64/libHepMC3rootIO.*`" != "x" ] ], 
       [AC_SUBST(HEPMC3,[$with_hepmc3])],
        AC_MSG_RESULT([Cannot find $with_hepmc3/lib/root/libHepMC3rootIO.* or $with_hepmc3/lib64/root/libHepMC3rootIO.* or $with_hepmc3/lib/libHepMC3rootIO.* or $with_hepmc3/lib64/libHepMC3rootIO.* libraries. 
        Specify correct path to HEPMC3 installation: --with-hepmc3=/path/to/hepmc3 and make sure the rootIO libraries are installed]) ; 
        AC_MSG_RESULT([Building RAPGAP without HepMC3rootIO]); AC_SUBST([no_hepmc3rootIO], [1]))
if test x$no_hepmc3rootIO == x; then     
  AC_SUBST([HEPMC3LIBROOTIO], [" -L$with_hepmc3/lib -L$with_hepmc3/lib64 -L$with_hepmc3/root/lib -L$with_hepmc3/lib64/root -lHepMC3 -lHepMC3rootIO"])
  CXXFLAGS="${CXXFLAGS} -DHEPMC3_ROOTIO=1 "
  AC_MSG_RESULT([Building RAPGAP with HepMC3rootIO: $HEPMC3LIBROOTIO])

fi  
fi  
AM_CONDITIONAL([NO_HEPMC3],test $no_hepmc3)     
  

#AM_CONDITIONAL([HZTOOL_OPT], [test "$hzexe" = yes])
   

if test "x$PDFLIB" = "x"; then
  AC_SUBST([PDFLIBSOURCE], ["pdfset.F   structm.F   structp.F"])
  AC_MSG_RESULT([using $PDFLIBSOURCE ])
else
  AC_SUBST([PDFLIBDSOURCE], [""])
  AC_MSG_RESULT([using pdfset.F   structm.F   structp.F from $lhapdf ])
fi

#AC_MSG_CHECKING([for ARIADNE installation])
#AC_ARG_WITH([ariadne],[AS_HELP_STRING([--with-ariadne],[path to ARIADNE installation])],[:],[with_ariadne=${ARIADNE_DIR}])
#
#if test "x$with_ariadne" != "x"; then
#	AC_SUBST(ARIADNE_DIR,[$with_ariadne]) 
#      AC_SUBST([ariadne], [1])
#      AS_IF([[ test "x`ls $with_ariadne/lib/libariadne412.*`" != "x" ||  test "x`ls $with_ariadne/lib64/libariadne412.*`" != "x"] ], #[AC_SUBST([ARIADNELIB], ["-L$with_ariadne/lib -L$with_ariadne/lib64 -lariadne412 "])],
#        AC_MSG_RESULT([Cannot find $with_ariadne/lib64/libariadne412.* or $with_ariadne/lib64/libariadne412.* libraries. 
#        Specify correct path to ARIADNE installation: --with-ariadne=/path/to/ariadne. Will try to search for ar4 libraries.]);)
#      AS_IF([[ test "x`ls $with_ariadne/lib/libar4.*`" != "x" ||  test "x`ls $with_ariadne/lib64/libar4.*`" != "x"] ], [AC_SUBST([ARIADNELIB], #["-L$with_ariadne/lib -L$with_ariadne/lib64 -lar4 "])],
#        AC_MSG_RESULT([Cannot find $with_ariadne/lib64/libariadne.* or $with_ariadne/lib64/libariadne.* libraries. 
#        Specify correct path to ARIADNE installation: --with-ariadne=/path/to/ariadne]);)
#      AC_MSG_RESULT([Building RAPGAP with ariadne: $ARIADNELIB ])
#      AC_SUBST([ariadne], [1])
#fi
#AM_CONDITIONAL([ARI_OPT], [test "$ariadne"])


#if test "x$with_ariadne" = "x"; then
#    AC_SUBST([ARIADNELIB], [" "])
#    AC_MSG_RESULT([Building RAPGAP without ariadne])
#fi

if test "x$F77" = "xg77"; then
  AC_SUBST([MYFFLAGS], ["-fno-automatic -finit-local-zero -fno-backslash -fno-globals -ff90 -fpic -ffixed-line-length-132"])
  AC_MSG_RESULT([Using g77 with FLAGS = $MYFFLAGS ])
else
  AC_SUBST([MYFFLAGS], ["-O0 -std=legacy -fno-automatic -fno-backslash -ffixed-line-length-132"]) 
  AC_MSG_RESULT([Using gfortran with FLAGS = $MYFFLAGS ])
fi



AC_CONFIG_FILES([
 Makefile
 include/Makefile
 misc/Makefile
 bases51/Makefile
 pythia6428/Makefile
 ariadne412/Makefile
 src/Makefile
 data/Makefile
 manual/Makefile 
])

AC_OUTPUT
