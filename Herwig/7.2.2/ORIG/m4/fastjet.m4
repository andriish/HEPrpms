dnl CHECK FASTJET BEGIN
dnl
dnl This script can be used in configure scripts to check for the
dnl usability of the FastJet librarty.
dnl
dnl By defaults, it searches the FastJet library in standard system
dnl locations but an alternative path can be specified using the
dnl --with-fastjet=... configure option
dnl
dnl If FastJet is found and functional, the variables FASTJET_CXXFLAGS
dnl and FASTJET_LIBS are set
dnl
dnl modified for Herwig 2011-10-04 D.Grellscheid
dnl
AC_DEFUN([FASTJET_CHECK_FASTJET],
[
dnl ckeck if a directory is specified for FastJet
AC_ARG_WITH(fastjet,
            [AC_HELP_STRING([--with-fastjet=dir], 
                            [Assume the given directory for FastJet])])

dnl search for the fastjet-config script
if test "$with_fastjet" = ""; then
   AC_PATH_PROG(fjconfig, fastjet-config, no)
else
   AC_PATH_PROG(fjconfig, fastjet-config, no, ${with_fastjet}/bin)
fi

LOAD_FASTJET=""
CREATE_FASTJET="#create"

if test "${fjconfig}" = "no"; then
   AC_MSG_CHECKING(FastJet)
   AC_MSG_RESULT(no);
   $2
else

   dnl now see if FastJet is functional
   save_CXXFLAGS="$CXXFLAGS"
   save_LIBS="$LIBS"

   CXXFLAGS="${CXXFLAGS} `${fjconfig} --cxxflags`"
   LIBS="${LIBS} `${fjconfig} --libs --plugins`"

   AC_MSG_CHECKING([if FastJet is functional])
   AC_LANG_PUSH(C++)
   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <fastjet/ClusterSequence.hh>
   ]], [[
fastjet::PseudoJet pj=fastjet::PtYPhiM(10.0,0.5,1.0,0.0);
   ]])], [fjok='yes'], [fjok='no'])
   AC_MSG_RESULT([$fjok])
   AC_LANG_POP()
   CXXFLAGS="$save_CXXFLAGS"
   LIBS="$save_LIBS"

   AC_MSG_CHECKING(FastJet)
   if test "${fjok}" = "yes"; then
      FASTJET_CXXFLAGS="`${fjconfig} --cxxflags`"
      FASTJET_LIBS="`${fjconfig} --libs --plugins`"
      LOAD_FASTJET="library HwLEPJetAnalysis.so"
      CREATE_FASTJET="create"
      AC_MSG_RESULT(yes)
      $1
   else
      AC_MSG_RESULT(no)
      $2
   fi

   fjcheckver=`${fjconfig} --version | sed s/'\.'//g`
   fjprefix=`${fjconfig} --prefix`
   if test "${host_cpu}" == "x86_64" -a $fjcheckver -le 301 -a -e `${fjconfig} --prefix`/lib64 ; then
     AC_MSG_WARN([
     ***************************************************************************
     FastJet <= 3.0.1 has been recognized. The FastJet libraries
     are located in 
       $fjprefix/lib64 which is not properly
     communicated by fastjet-config. Consider to configure FastJet with
       --libdir=$fjprefix/lib
     ***************************************************************************
     ])
   fi

fi


AC_SUBST(FASTJETINCLUDE,[$FASTJET_CXXFLAGS])
AC_SUBST(CREATE_FASTJET)
AC_SUBST(LOAD_FASTJET)
AC_SUBST(FASTJETLIBS,[${FASTJET_LIBS/-Wl,-rpath,/-R}])

AM_CONDITIONAL(WANT_LIBFASTJET,[test "x$CREATE_FASTJET" = "xcreate"])
])

dnl CHECK FASTJET END
