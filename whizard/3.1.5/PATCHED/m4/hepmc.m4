dnl hepmc.m4 -- checks for HepMC library
dnl

### Determine paths to HEPMC2/3 components
### If successful, set the conditional HEPMC3_AVAILABLE
### Also: HEPMC2/3_VERSION HEPMC2/3_INCLUDES LDFLAGS2/3_HEPMC
AC_DEFUN([WO_PROG_HEPMC],
[dnl
AC_REQUIRE([AC_PROG_CXX])
AC_REQUIRE([AC_PROG_FC])

AC_ARG_ENABLE([hepmc2],
  [AS_HELP_STRING([--enable-hepmc2],
    [enable HepMC2 for handling event data [[yes]]])],
  [], [enable_hepmc2="yes"])

AC_ARG_ENABLE([hepmc],
  [AS_HELP_STRING([--enable-hepmc],
    [enable HepMC3 for handling event data [[yes]]])],
  [], [enable_hepmc="yes"])

hepmcok="no"

if test "$enable_hepmc" = "yes"; then
   AC_MSG_NOTICE([looking for HepMC3 ... ])
   ACX_CHECK_HEPMC3()
   if test "${hepmcok}" = "no"; then
     AC_MSG_NOTICE([HepMC3 not found, incompatible, or HepMC3-config not found])
     AC_MSG_CHECKING([for HepMC3])
     AC_MSG_RESULT([(disabled)])
     enable_hepmc="no"
     AM_CONDITIONAL([HEPMC3_HAS_ROOT],false)
   else
      AC_MSG_CHECKING([the HepMC3 version])
      save_CXXFLAGS="$CXXFLAGS"
      save_LIBS="$LIBS"
      CXXFLAGS="${CXXFLAGS} `${hepmcconfig} --cxxflags`"
      LIBS="${LIBS} -Wl,-rpath,`${hepmcconfig} --libdir` `${hepmcconfig} --libs`"
      AC_LANG([C++])
      AC_LINK_IFELSE([dnl
        AC_LANG_PROGRAM([[
#include <stdio.h>
#include <iostream>
#include "HepMC3/Version.h"
]],
          [[using namespace HepMC3; std::cout << HepMC3::version();]])],
        [dnl
        wk_hepmc3_version=`./conftest`
        AC_MSG_RESULT([$wk_hepmc3_version])],
        [dnl
        AC_MSG_RESULT([unknown])
        enable_hepmc="no"])  
      CXXFLAGS="$save_CXXFLAGS"
      LIBS="$save_LIBS"  
      HEPMC3_VERSION=$wk_hepmc3_version
      AC_SUBST([HEPMC3_VERSION])   
   fi
fi

AC_SUBST([HEPMC3_INCLUDES])
AC_SUBST([LDFLAGS_HEPMC3])

AM_CONDITIONAL([HEPMC3_AVAILABLE], [test "$enable_hepmc" = "yes"])

if test "$enable_hepmc" = "yes"; then
   HEPMC3_AVAILABLE_FLAG=".true."
else
   HEPMC3_AVAILABLE_FLAG=".false."
fi
if test "$enable_hepmc" = "no"; then
   AM_CONDITIONAL([HEPMC3_HAS_ROOT],false)
fi
AC_SUBST([HEPMC3_AVAILABLE_FLAG])

if test "$enable_hepmc2" = "yes"; then
   AC_MSG_NOTICE([looking for HepMC2 ... ])
   if test -n "$HEPMC2_DIR"; then
     wo_hepmc2_includes="-I$HEPMC2_DIR/include"
   fi
   AC_MSG_CHECKING([the HepMC2 version])
   AC_LANG([C++])
   wo_cxxflags_tmp=$CXXFLAGS
   CXXFLAGS="$CXXFLAGS $wo_hepmc2_includes"
   AC_LINK_IFELSE([dnl
     AC_LANG_PROGRAM([[#include "HepMC/Version.h"]],
       [[std::cout << HepMC::versionName();]])],
     [dnl
     wk_hepmc2_version=`./conftest`
     AC_MSG_RESULT([$wk_hepmc2_version])],
     [dnl
     AC_MSG_RESULT([unknown])
     enable_hepmc="no"])
   CXXFLAGS=$wo_cxxflags_tmp
   
   HEPMC2_VERSION=$wk_hepmc2_version
   AC_SUBST([HEPMC2_VERSION])

   if test -n "$HEPMC2_DIR"; then
     wo_hepmc2_ldflags="-Wl,-rpath,$HEPMC2_DIR/lib -L$HEPMC2_DIR/lib -lHepMC"
   else
     wo_hepmc2_ldflags="-lHepMC"
   fi
   if test "$enable_hepmc2" = "yes"; then
     wo_require_stdcpp="yes"
     AC_MSG_CHECKING([for GenEvent class in -lHepMC])
     wo_libs_tmp=$LIBS
     LIBS="$wo_hepmc2_ldflags $wo_libs_tmp"
     AC_LANG([C++])
     wo_cxxflags_tmp=$CXXFLAGS
     CXXFLAGS="$CXXFLAGS $wo_hepmc2_includes"
     AC_LINK_IFELSE([dnl
       AC_LANG_PROGRAM([[#include "HepMC/GenEvent.h"]],
         [[using namespace HepMC;  GenEvent* evt = new GenEvent();]])],
       [],
       [enable_hepmc2="no"])
     AC_MSG_RESULT([$enable_hepmc2])
     CXXFLAGS=$wo_cxxflags_tmp
     LIBS=$wo_libs_tmp
   else
     AC_MSG_CHECKING([for HepMC2])
     AC_MSG_RESULT([(disabled)])
   fi
   
   if test "$enable_hepmc2" = "yes"; then
     HEPMC2_INCLUDES=$wo_hepmc2_includes
     LDFLAGS_HEPMC2=$wo_hepmc2_ldflags
   fi
fi

AC_SUBST([HEPMC2_INCLUDES])
AC_SUBST([LDFLAGS_HEPMC2])

AM_CONDITIONAL([HEPMC2_AVAILABLE], [test "$enable_hepmc2" = "yes"])
if test "$enable_hepmc2" = "yes"; then
   HEPMC2_AVAILABLE_FLAG=".true."
else
   HEPMC2_AVAILABLE_FLAG=".false."
fi
AC_SUBST([HEPMC2_AVAILABLE_FLAG])
])

dnl #####################################################################
dnl CHECK HEPMC3 BEGIN
dnl
dnl This script can be used in configure scripts to check for the
dnl usability of the HepMC3 library.
dnl
dnl By defaults, it searches the HepMC3 library in standard system
dnl locations but an alternative path can be specified using the
dnl --with-hepmc=... configure option
dnl
dnl If HepMC3 is found and functional, the variables HEPMC_CXXFLAGS
dnl and HEPMC_LIBS are set
AC_DEFUN([ACX_CHECK_HEPMC3],
[
dnl ckeck if a directory is specified for HepMC
AC_ARG_WITH(HepMC,
            [AS_HELP_STRING([--with-hepmc=dir],
                            [assume the given directory for HepMC])])

dnl search for the Hepmc3-config script
if test "$with_hepmc" = ""; then
   AC_PATH_PROG(hepmcconfig, HepMC3-config, no)
else
   AC_PATH_PROG(hepmcconfig, HepMC3-config, no, ${with_hepmc}/bin)
fi

if test "${hepmcconfig}" = "no"; then
   AC_MSG_CHECKING([for HepMC3 or newer])
   AC_MSG_RESULT(no);
   $2
else

   dnl now see if HepMC3 is functional
   save_CXXFLAGS="$CXXFLAGS"
   save_LIBS="$LIBS"

   CXXFLAGS="${CXXFLAGS} `${hepmcconfig} --cxxflags`"
   LIBS="${LIBS} `${hepmcconfig} --libs`"

   AC_MSG_CHECKING([if HepMC3 is built with ROOT interface])
   if ${hepmcconfig} --rootIO | grep rootIO >/dev/null 2>&1; then
      hepmc3_root="yes"
      HEPMCROOTLIBS="`${hepmcconfig} --rootIO` -Wl,-rpath,$ROOTLIBDIR -L$ROOTLIBDIR $ROOTLIBS"
      HEPMCROOTINCL="-I$ROOTINCDIR"
   else
      hepmc3_root="no"
   fi
   AC_MSG_RESULT([$hepmc3_root])
   AM_CONDITIONAL([HEPMC3_HAS_ROOT], [test "$hepmc3_root" = "yes"])
   if test "$hepmc3_root" = "yes"; then
      AC_DEFINE([HEPMC3_ROOTIO])
   fi

   AC_MSG_CHECKING([if HepMC3 is functional])
   AC_LANG_PUSH(C++)
   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <HepMC3/GenEvent.h>
   ]], [[
using namespace HepMC3; GenEvent evt(Units::GEV,Units::MM);
   ]])], [hepmcok='yes'], [hepmcok='no'])
   AC_MSG_RESULT([$hepmcok])
   AC_LANG_POP()
   CXXFLAGS="$save_CXXFLAGS"
   LIBS="$save_LIBS"

   AC_MSG_CHECKING([for HepMC3])
   if test "${hepmcok}" = "yes"; then
      HEPMC3_INCLUDES="`${hepmcconfig} --cxxflags` $HEPMCROOTINCL"
      LDFLAGS_HEPMC3="-Wl,-rpath,`${hepmcconfig} --libdir` `${hepmcconfig} --libs` $HEPMCROOTLIBS"
      AC_MSG_RESULT([yes])
      $1
   else
      AC_MSG_RESULT([no])
      $2
   fi
fi
])

dnl CHECK HEPMC3 END
