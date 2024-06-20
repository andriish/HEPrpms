## Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([Rivet],[4.0.0],[rivet@projects.hepforge.org],[Rivet])

## Check and block installation into the src/build dir
if test "$prefix" = "$PWD"; then
  AC_MSG_ERROR([Installation into the build directory is not supported: use a different --prefix argument])
fi
## Force default prefix to have a path value rather than NONE
if test "$prefix" = "NONE"; then
   prefix=/usr/local
fi

AC_CONFIG_SRCDIR([src/Core/Analysis.cc])
AC_CONFIG_HEADERS([include/Rivet/Config/DummyConfig.hh include/Rivet/Config/RivetConfig.hh])
AM_INIT_AUTOMAKE([dist-bzip2 -Wall 1.10])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])
m4_ifdef([AM_PROG_AR], [AM_PROG_AR])
AC_CONFIG_MACRO_DIR([m4])
AC_SUBST(LT_OBJDIR)

## Compute version code
let rivet_major=[`echo "$PACKAGE_VERSION" | cut -d. -f1 | sed -e 's/^0*//'`]
let rivet_minor=[`echo "$PACKAGE_VERSION" | cut -d. -f2 | sed -e 's/^0*//'`]
let rivet_micro=[`echo "$PACKAGE_VERSION" | cut -d. -f3 | sed -e 's/^0*//'`]
let PACKAGE_VERSION_CODE=10000*$rivet_major+100*$rivet_minor+$rivet_micro

## Package-specific #defines
AC_DEFINE_UNQUOTED(RIVET_VERSION, "$PACKAGE_VERSION", "Rivet version string")
AC_DEFINE_UNQUOTED(RIVET_VERSION_CODE, $PACKAGE_VERSION_CODE, "Rivet version int")
AC_DEFINE_UNQUOTED(RIVET_NAME, "$PACKAGE_NAME", "Rivet name string")
AC_DEFINE_UNQUOTED(RIVET_STRING, "$PACKAGE_STRING", "Rivet name and version string")
AC_DEFINE_UNQUOTED(RIVET_TARNAME, "$PACKAGE_TARNAME", "Rivet short name string")
AC_DEFINE_UNQUOTED(RIVET_BUGREPORT, "$PACKAGE_BUGREPORT", "Rivet contact email address")
AC_DEFINE_UNQUOTED(RIVET_SUPPORTS_MERGING_IN_MEMORY, 1, "Rivet memory-based merging support")



## OS X
AC_CEDAR_OSX

## Work out the LCG platform tag
dnl AC_LCG_TAG

## Set default compiler flags
if test "x$CXXFLAGS" == "x"; then CXXFLAGS="-O2"; fi

## Compiler setup
AC_LANG(C++)
AC_PROG_CXX
AX_CXX_COMPILE_STDCXX([17], [noext], [mandatory])

## Store and propagate the compiler identity and flags
RIVETCXX="$CXX"
AC_SUBST(RIVETCXX)
RIVETCXXFLAGS="$CXXFLAGS"
AC_SUBST(RIVETCXXFLAGS)
RIVETCPPFLAGS="$CPPFLAGS"
AC_SUBST(RIVETCPPFLAGS)
RIVETLIBS="$LIBS"
AC_SUBST(RIVETLIBS)
RIVETLDFLAGS="$LDFLAGS"
AC_SUBST(RIVETLDFLAGS)

## Checks for programs.
AC_PROG_INSTALL
AC_PROG_LN_S
AC_DISABLE_STATIC
LT_INIT([dlopen])
AX_EXECINFO
AC_FUNC_STRERROR_R
AC_CHECK_PROG([FCP], [rsync], [rsync -aq], [cp -f])
AC_CHECK_PROG([RSH], [rsync], [rsync], [scp])

## YODA histogramming library
AC_CEDAR_LIBRARYANDHEADERS([YODA], , , [AC_MSG_ERROR([YODA is required])])
YODABINPATH=$YODALIBPATH/../bin
AC_SUBST(YODABINPATH)
AC_PATH_PROG(YODACONFIG, yoda-config, [], [$YODALIBPATH/../bin:$PATH])
YODA_PYTHONPATH=""
if test -f "$YODACONFIG"; then
  AC_MSG_CHECKING([YODA version using yoda-config])
  YODA_VERSION=`$YODACONFIG --version`
  AC_MSG_RESULT([$YODA_VERSION])
  YODA_VERSION1=[`echo $YODA_VERSION | cut -d. -f1 | sed -e 's/^0*//' -e 's/\([0-9]*\).*/\1/g'`]
  YODA_VERSION2=[`echo $YODA_VERSION | cut -d. -f2 | sed -e 's/^0*//' -e 's/\([0-9]*\).*/\1/g'`]
  YODA_VERSION3=[`echo $YODA_VERSION | cut -d. -f3 | sed -e 's/^0*//' -e 's/\([0-9]*\).*/\1/g'`]
  let YODA_VERSION_INT=YODA_VERSION1*10000+YODA_VERSION2*100+YODA_VERSION3
  if test $YODA_VERSION_INT -lt 20000; then
    AC_MSG_ERROR([YODA version isn't sufficient: at least version 2.0.0 required])
  fi
  AC_MSG_CHECKING([YODA Python path using yoda-config])
  YODA_PYTHONPATH=`$YODACONFIG --pythonpath`
  YODACPPFLAGS=`$YODACONFIG --cppflags`
  AC_MSG_RESULT([$YODA_PYTHONPATH])
fi
AC_SUBST(YODACPPFLAGS)
AC_SUBST(YODA_PYTHONPATH)


## HepMC event record library
use_hepmc=no
AC_ARG_WITH([hepmc3],
            [AS_HELP_STRING(--with-hepmc3, path to HepMC3 @<:@$prefix and various standard locations@:>@)],
            [HEPMC3PATH=$with_hepmc3], [])
AC_ARG_WITH([hepmc],
            [AS_HELP_STRING(--with-hepmc, path to HepMC3 @<:@$prefix and various standard locations@:>@)],
            [HEPMC3PATH=$with_hepmc], [])
AC_CEDAR_LIBRARYANDHEADERS([HepMC3], , [use_hepmc=yes], [use_hepmc=no])
if test x$use_hepmc = xno ; then
  AC_MSG_ERROR([ HepMC3 is required])
fi

## Test the discovered HepMC
rm -f conftest.*
oldCPPFLAGS=$CPPFLAGS
CPPFLAGS="$CPPFLAGS -I$HEPMC3INCPATH"
AC_LANG_CONFTEST([AC_LANG_SOURCE([#include <iostream>
#include "HepMC3/Version.h"
int main() { std::cout << HepMC3::version() << std::endl; return 0; }])])
if test -f conftest.cc; then
  $CXX $CPPFLAGS conftest.cc -o conftest 2>&1 1>&5
elif test -f conftest.C; then
  $CXX $CPPFLAGS conftest.C -o conftest 2>&1 1>&5
else
  $CXX $CPPFLAGS conftest.cpp -o conftest 2>&1 1>&5
fi
hepmc_version=`./conftest`
if test x$hepmc_version != x; then
  let hepmc_major=[`echo "$hepmc_version" | cut -d. -f1 | sed -e 's/^0//'`]
  let hepmc_minor=[`echo "$hepmc_version" | cut -d. -f2 | sed -e 's/^0//'`]
  let hepmc_third=[`echo "$hepmc_version" | cut -d. -f3 | sed -e 's/^0//'`]
fi
rm -f conftest conftest.cpp conftest.cc conftest.C
HEPMC_VERSION=$hepmc_major$hepmc_minor$hepmc_third
AC_MSG_NOTICE([HepMC version is $hepmc_version -> $HEPMC_VERSION])
if test $HEPMC_VERSION -le 310; then
  AC_MSG_ERROR([HepMC3 version 3.1.1 or later is required.])
fi

AC_SUBST(HEPMC_VERSION)
CPPFLAGS=$oldCPPFLAGS

## Use generic HEPMC* variables for HepMC3 as well (needed for rivet-config.in)
c=$HEPMC3INCPATH
HEPMCINCPATH=$HEPMC3INCPATH
HEPMCLIBPATH=$HEPMC3LIBPATH
HEPMCLIBNAME="-lHepMC3 -lHepMC3search"
AC_SUBST(HEPMCINCPATH)
AC_SUBST(HEPMCLIBPATH)
AC_SUBST(HEPMCLIBNAME)

CPPFLAGS="$CPPFLAGS -DENABLE_HEPMC_3=true"

## FastJet clustering library
AC_CEDAR_LIBRARYANDHEADERS([fastjet], , , [AC_MSG_ERROR([FastJet is required])])
AC_PATH_PROG(FJCONFIG, fastjet-config, [], $FASTJETPATH/bin:$PATH)
if test -f "$FJCONFIG"; then
  AC_MSG_CHECKING([FastJet version using fastjet-config])
  fjversion=`$FJCONFIG --version`
  AC_MSG_RESULT([$fjversion])
  fjmajor=[`echo $fjversion | cut -f1 -d. | sed -e 's/^0*//'`]
  fjminor=[`echo $fjversion | cut -f2 -d. | sed -e 's/^0*//'`]
  fjmicro=[`echo $fjversion | cut -f3 -d. | sed -e 's/^0*//'`]
  if test "$fjmajor" -lt 3 -o "$fjminor" -lt 2 ; then  #< only works for v3.x
    AC_MSG_ERROR([FastJet version 3.2.0 or later is required])
  fi
  FASTJETLIBADD="$($FJCONFIG --libs --plugins --rpath=no)"
else
  FASTJETLIBADD="-L$FASTJETLIBPATH -l$FASTJETLIBNAME -lfastjetplugins"
fi
FASTJETLDFLAGS="-L$FASTJETLIBPATH"


## FastJet contrib (allow paths specified separately from FJ)
AC_ARG_WITH([fjcontrib],
            [AS_HELP_STRING([--with-fjcontrib],[path to FastJet Contrib @<:@$FASTJETPATH@:>@])], [], [])
if test -n "$with_fjcontrib"; then
  AC_MSG_NOTICE([FastJet Contrib base path given: $with_fjcontrib])
  FJCONTRIBINCPATH=$with_fjcontrib/include
  FJCONTRIBLIBPATH=$with_fjcontrib/lib
  if test -f "$with_fjcontrib/lib64/libfastjettools.so" -o -f "$with_fjcontrib/lib64/libfastjettools.dyld"; then
    FJCONTRIBLIBPATH=$with_fjcontrib/lib64
  fi
  FJCONTRIBCPPFLAGS=-I$FJCONTRIBINCPATH
  AC_SUBST(FJCONTRIBINCPATH)
  AC_SUBST(FJCONTRIBLIBPATH)
  AC_SUBST(FJCONTRIBCPPFLAGS)
  FASTJETLIBADD="$FASTJETLIBADD -L$FJCONTRIBLIBPATH"
  FASTJETLDFLAGS="$FASTJETLDFLAGS -L$FJCONTRIBLIBPATH"
else
  FJCONTRIBINCPATH=$FASTJETINCPATH
  FJCONTRIBLIBPATH=$FASTJETLIBPATH
fi
FASTJETLIBADD="$FASTJETLIBADD -lfastjetcontribfragile -lfastjettools"
AC_SUBST(FASTJETLIBADD)
AC_SUBST(FASTJETLDFLAGS)
AC_MSG_NOTICE([FastJet lib flags: $FASTJETLIBADD])
AC_MSG_NOTICE([FastJet ld flags: $FASTJETLDFLAGS])

## Test FastJet + Contrib
## Check for FastJet headers that require the --enable-all(cxx)plugins option
oldCPPFLAGS=$CPPFLAGS
CPPFLAGS="$CPPFLAGS $FASTJETCPPFLAGS $FJCONTRIBCPPFLAGS"
oldLIBS=$LIBS
LIBS="$LIBS $FASTJETLIBADD"
FASTJET_ERRMSG="Required FastJet plugin headers were not found: did you build FastJet with the --enable-allcxxplugins option?"
AC_CHECK_HEADER([fastjet/D0RunIIConePlugin.hh], [], [AC_MSG_ERROR([$FASTJET_ERRMSG])])
AC_CHECK_HEADER([fastjet/TrackJetPlugin.hh], [], [AC_MSG_ERROR([$FASTJET_ERRMSG])])
FASTJET_ERRMSG="Required FastJet Contrib headers were not found: did you install FastJet Contrib?"
AC_CHECK_HEADER([fastjet/contrib/SoftDrop.hh], [], [AC_MSG_ERROR([$FASTJET_ERRMSG])])
AC_MSG_CHECKING([FastJet test program compilation])
FASTJET_ERRMSG="Couldn't build FastJet test program: are your FastJet and FastJet Contrib versions up to date, and did you install Contrib with 'make fragile-shared-install'?"
AC_LINK_IFELSE([AC_LANG_SOURCE([#include <vector>
#include "fastjet/PseudoJet.hh"
#include "fastjet/ClusterSequence.hh"
#include "fastjet/contrib/SoftDrop.hh"
int main() {
  std::vector<fastjet::PseudoJet> particles{{1,1,0,2}, {1,-1,0,2}};
  fastjet::ClusterSequence cs(particles, fastjet::JetDefinition(fastjet::antikt_algorithm, 0.4));
  fastjet::contrib::SoftDrop sd(0.5, 0.1);
  for (const fastjet::PseudoJet& j : cs.inclusive_jets()) {
    const fastjet::PseudoJet& jsd = sd(j);
  }
  return 0;
}
])],
  [AC_MSG_RESULT([successful])],
  [AC_MSG_RESULT([failed])
   AC_MSG_ERROR([$FASTJET_ERRMSG])])
CPPFLAGS=$oldCPPFLAGS
LIBS=$oldLIBS

## HDF5 + highfive
AX_LIB_HDF5()
AC_MSG_NOTICE([HDF5 include flags: $HDF5_CFLAGS ::: $HDF5_CPPFLAGS])
AC_MSG_NOTICE([HDF5 lib flags: $HDF5_LDFLAGS ::: $HDF5_LIBS])
if test x$HDF5_TYPE == xparallel; then
  AX_PROG_CXX_MPI([true], [], [AC_MSG_ERROR([Found hdf5-parallel, but no MPI compiler.])])
fi
#
AC_CEDAR_HEADERS([highfive], , , [AC_MSG_ERROR([highfive (HDF5 for C++) is required])])
oldCPPFLAGS=$CPPFLAGS
CPPFLAGS="$CPPFLAGS $HDF5_CPPFLAGS $HIGHFIVECPPFLAGS"
AC_CHECK_HEADER([highfive/H5File.hpp], [], [AC_MSG_ERROR([H5 header not found.])])
CPPFLAGS=$oldCPPFLAGS

## HEPTopTagger
# AC_CEDAR_HEADERS([HEPTopTagger], , , [AC_MSG_ERROR([HEPTopTagger is required])])


# ## GNU Scientific Library
# AC_SEARCH_GSL
# AC_CEDAR_HEADERS([gsl], , , [AC_MSG_ERROR([GSL (GNU Scientific Library) is required])])
# oldCPPFLAGS=$CPPFLAGS
# CPPFLAGS="$CPPFLAGS -I$GSLINCPATH"
# AC_CHECK_HEADER([gsl/gsl_vector.h], [], [AC_MSG_ERROR([GSL vectors not found.])])
# CPPFLAGS=$oldCPPFLAGS


## Disable build/install of standard analyses
AC_ARG_ENABLE([analyses],
  [AS_HELP_STRING([--disable-analyses],[don't try to build or install standard analyses])],
  [], [enable_analyses=yes])
if test x$enable_analyses != xyes; then
   AC_MSG_WARN([Not building standard Rivet analyses, by request])
fi
AM_CONDITIONAL(ENABLE_ANALYSES, [test x$enable_analyses = xyes])


## Optional build/install of ONNXRuntime analyses (make standard at some point)
AC_ARG_ENABLE([onnxrt],
  [AS_HELP_STRING([--enable-onnxrt],
  [require ONNXRuntime library and build ONNX analyses plugin; optionally give the path to ONNXRuntime @<:@$prefix@:>@])],
  [enable_onnx=yes; ONNXRTPREFIX=$enable_onnxrt], [enable_onnx=no; ONNXRTPREFIX=])
if test x$enable_onnx != xno; then
  AC_MSG_WARN([Building with ONNXRuntime support, and including ONNX-analyses plugin])
  if test x$ONNXRTPREFIX = xyes; then ONNXRTPREFIX=$prefix; fi
  # TODO: test ONNXRt functionality from given paths
  ONNXRTINCPATH=$ONNXRTPREFIX/include
  ONNXRTLIBPATH=$ONNXRTPREFIX/lib
fi
AM_CONDITIONAL(ENABLE_ONNX, [test x$enable_onnx = xyes])
AC_SUBST(ONNXRTPREFIX)
AC_SUBST(ONNXRTINCPATH)
AC_SUBST(ONNXRTLIBPATH)

dnl ## Build LaTeX docs if possible...
dnl AC_PATH_PROG(PDFLATEX, pdflatex)
dnl AM_CONDITIONAL(WITH_PDFLATEX, [test x$PDFLATEX != x])
dnl ## ... unless told otherwise!
dnl AC_ARG_ENABLE([pdfmanual],
dnl   [AS_HELP_STRING([--enable-pdfmanual],[build and install the manual])],
dnl   [], [enable_pdfmanual=no])
dnl if test x$enable_pdfmanual = xyes; then
dnl    AC_MSG_WARN([Building Rivet manual, by request])
dnl fi
# TODO: remove
AM_CONDITIONAL(WITH_PDFLATEX, [test x$PDFLATEX != x])
AM_CONDITIONAL(ENABLE_PDFMANUAL, [test x$enable_pdfmanual = xyes])
#
AC_ARG_ENABLE([pdfmanual],
  [AS_HELP_STRING([--enable-pdfmanual],[backward compatility dummy flag, no effect and deprecated])], [], [])


## Build Doxygen documentation if possible
AC_ARG_ENABLE([doxygen],
  [AS_HELP_STRING([--disable-doxygen],[don't try to make Doxygen documentation])],
  [], [enable_doxygen=yes])
if test x$enable_doxygen = xyes; then
   AC_PATH_PROG(DOXYGEN, doxygen)
fi
AM_CONDITIONAL(WITH_DOXYGEN, [test x$DOXYGEN != x])

## Build asciidoc docs if possible
AC_PATH_PROG(ASCIIDOC, asciidoc)
AM_CONDITIONAL(WITH_ASCIIDOC, [test x$ASCIIDOC != x])


## Python extension
AC_ARG_ENABLE(pyext, [AS_HELP_STRING([--disable-pyext],[don't build Python module (default=build)])],
  [], [enable_pyext=yes])
## Basic Python checks
if test x$enable_pyext == xyes; then
  AM_PATH_PYTHON
  AX_PYTHON_DEVEL([>= '2.7.3'])
  PYTHON_FULL_VERSION=`$PYTHON -c 'from __future__ import print_function; import platform; print(platform.python_version())'`
  PYTHON_MAJOR_VERSION=`$PYTHON -c 'from __future__ import print_function; import sys; print(sys.version_info.major)'`
  AC_SUBST(PYTHON_VERSION)
  AC_SUBST(PYTHON_FULL_VERSION)
  AC_SUBST(PYTHON_MAJOR_VERSION)
  #RIVET_PYTHONPATH=$pythondir
  RIVET_PYTHONPATH=`$PYTHON -c "from __future__ import print_function; import sysconfig; print(sysconfig.get_path('platlib', 'posix_prefix', vars={'platbase': 'XXX', 'base': 'XXX'}).replace('/local', '').replace('XXX', '$prefix'))"`
  AC_SUBST(RIVET_PYTHONPATH)
  AC_MSG_NOTICE(Rivet Python lib will be installed to $RIVET_PYTHONPATH)
  if test -z "$PYTHON"; then
    AC_MSG_ERROR([Can't build Python extension since Python executable can't be found])
    enable_pyext=no
  fi
  if test -z "$PYTHON_CPPFLAGS"; then
    AC_MSG_ERROR([Can't build Python extension since Python.h header file cannot be found])
    enable_pyext=no
  fi
fi
AM_CONDITIONAL(ENABLE_PYEXT, [test x$enable_pyext == xyes])


## Cython checks
if test x$enable_pyext == xyes; then
  AM_CHECK_CYTHON([0.24.0], [:], [:])
  if test x$CYTHON_FOUND = xyes; then
    AC_PATH_PROGS(CYTHON, [$CYTHON cython-$PYTHON_VERSION cython$PYTHON_VERSION cython-$PYTHON_MAJOR_VERSION cython$PYTHON_MAJOR_VERSION cython])
    if test x$CYTHON != x; then
        AC_MSG_NOTICE([Cython >= 0.24 found at $CYTHON: Python extension source can be rebuilt (for developers)])
        # Force rebuild since we have a sufficient Cython
        for i in pyext/rivet/*.pyx; do touch $i; done
    fi
  fi

  AC_CHECK_FILE([pyext/rivet/core.cpp],
                [],
                [if test "x$CYTHON_FOUND" != "xyes"; then
                  AC_MSG_ERROR([Cython is required for --enable-pyext, no pre-built core.cpp was found.])
                fi])

  cython_compiler=$CXX
  ## Set extra Python extension build flags (to cope with Cython output code oddities)
  PYEXT_CXXFLAGS="$CXXFLAGS"
  AC_CEDAR_CHECKCXXFLAG([-Wno-unused-but-set-variable], [PYEXT_CXXFLAGS="$PYEXT_CXXFLAGS -Wno-unused-but-set-variable"])
  AC_CEDAR_CHECKCXXFLAG([-Wno-sign-compare], [PYEXT_CXXFLAGS="$PYEXT_CXXFLAGS -Wno-sign-compare"])
  AC_SUBST(PYEXT_CXXFLAGS)
  AC_MSG_NOTICE([All Python build checks successful: 'rivet' Python extension will be built])
fi
AM_CONDITIONAL(WITH_CYTHON, [test x$CYTHON_FOUND = xyes])


## Set default build flags
AM_CPPFLAGS="-I\$(top_srcdir)/include -I\$(top_builddir)/include"
#AM_CPPFLAGS="$AM_CPPFLAGS -I\$(top_srcdir)/include/eigen3"
#AM_CPPFLAGS="$AM_CPPFLAGS \$(GSL_CPPFLAGS)"
AM_CPPFLAGS="$AM_CPPFLAGS \$(RIVETCPPFLAGS)"
AM_CPPFLAGS="$AM_CPPFLAGS \$(YODACPPFLAGS)"
AM_CPPFLAGS="$AM_CPPFLAGS -I\$(HEPMC3INCPATH)"
AM_CPPFLAGS="$AM_CPPFLAGS \$(FASTJETCPPFLAGS) \$(FJCONTRIBCPPFLAGS)"
AM_CPPFLAGS="$AM_CPPFLAGS \$(HDF5_CPPFLAGS) \$(HIGHFIVECPPFLAGS)"
AC_CEDAR_CHECKCXXFLAG([-pedantic], [AM_CXXFLAGS="$AM_CXXFLAGS -pedantic"])
AC_CEDAR_CHECKCXXFLAG([-Wall], [AM_CXXFLAGS="$AM_CXXFLAGS -Wall"])
AC_CEDAR_CHECKCXXFLAG([-Wunused-variable], [AM_CXXFLAGS="$AM_CXXFLAGS -Wunused-variable"])
AC_CEDAR_CHECKCXXFLAG([-Wno-long-long], [AM_CXXFLAGS="$AM_CXXFLAGS -Wno-long-long"])
AC_CEDAR_CHECKCXXFLAG([-Wno-format], [AM_CXXFLAGS="$AM_CXXFLAGS -Wno-format"])
AC_CEDAR_CHECKCXXFLAG([-Werror=uninitialized], [AM_CXXFLAGS="$AM_CXXFLAGS -Werror=uninitialized"])
AC_CEDAR_CHECKCXXFLAG([-Werror=delete-non-virtual-dtor], [AM_CXXFLAGS="$AM_CXXFLAGS -Werror=delete-non-virtual-dtor"])

## Add OpenMP-enabling flags if possible
AX_OPENMP([AM_CXXFLAGS="$AM_CXXFLAGS $OPENMP_CXXFLAGS"])

## Optional zlib support for gzip-compressed data streams/files
AX_CHECK_ZLIB


## Debug flag (default=-DNDEBUG, enabled=-g)
AC_ARG_ENABLE([debug], [AS_HELP_STRING([--enable-debug],[build with debugging symbols  @<:@default=no@:>@])], [], [enable_debug=no])
if test x$enable_debug == xyes; then
  AM_CXXFLAGS="$AM_CXXFLAGS -g"
fi

## Extra warnings flag (default=none)
AC_ARG_ENABLE([extra-warnings], [AS_HELP_STRING([--enable-extra-warnings],[build with extra compiler warnings (recommended for developers)  @<:@default=no@:>@])], [], [enable_extra_warnings=no])
if test x$enable_extra_warnings == xyes; then
  AC_MSG_NOTICE([Enabling extra compiler warnings])
  dnl AC_CEDAR_CHECKCXXFLAG([-Wextra -Wno-unused-variable], [AM_CXXFLAGS="$AM_CXXFLAGS -Wextra -Wno-unused-variable"])
  dnl AC_CEDAR_CHECKCXXFLAG([-Wdeprecated-copy], [AM_CXXFLAGS="$AM_CXXFLAGS -Wdeprecated-copy"])
  AC_CEDAR_CHECKCXXFLAG([-Wextra], [AM_CXXFLAGS="$AM_CXXFLAGS -Wextra "])
fi


AC_SUBST(AM_CPPFLAGS)
AC_SUBST(AM_CXXFLAGS)

AC_EMPTY_SUBST
AC_CONFIG_FILES(Makefile)
AC_CONFIG_FILES(include/Makefile include/Rivet/Makefile)
AC_CONFIG_FILES(src/Makefile)
AC_CONFIG_FILES(src/Core/Makefile src/Core/yamlcpp/Makefile)
AC_CONFIG_FILES(src/Tools/Makefile)
AC_CONFIG_FILES(src/Projections/Makefile)
AC_CONFIG_FILES(src/AnalysisTools/Makefile)
AC_CONFIG_FILES(analyses/Makefile)
AC_CONFIG_FILES(test/Makefile)
AC_CONFIG_FILES(pyext/Makefile pyext/rivet/Makefile pyext/build.py)
AC_CONFIG_FILES(doc/Makefile doc/doxygen/Doxyfile)
AC_CONFIG_FILES(bin/Makefile bin/rivet-config bin/rivet-build)
AC_CONFIG_FILES(rivetenv.sh rivetenv.csh rivet.pc)

AC_OUTPUT

if test x$enable_pyrivet == xyes; then
   cat <<EOF

************************************************************
RIVET CONFIGURED!

Now build and install (to the $prefix tree) with e.g.
make -j2 && make -j2 install

To use Rivet, we recommend reading HepMC files from a file
or pipe (the latter may be made with mkfifo) using the
'rivet' executable.

For a more pleasant command line experience, you can include
the bin/rivet-completion file into your .bashrc file,
or your bash_completion.d directory if you have one.

The rivetenv.*sh files will not be installed, but can help you
to set up a Rivet runtime environment in future.
************************************************************
EOF
fi
