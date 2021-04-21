AC_INIT([BlackHat], [0.9.9], [daniel.maitre@durham.ac.uk])
AM_INIT_AUTOMAKE([-Wall  foreign])
AM_DISABLE_STATIC
#AM_PROG_AR
m4_ifdef([AM_PROG_AR], [AM_PROG_AR])
AC_PROG_CXX
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_HEADERS([config.h])
AC_CHECK_FUNCS([pow log sqrt])
AC_CHECK_FUNCS([dlopen])
AC_MSG_CHECKING(whether the host is a Mac)
AC_CHECK_LIB([dl],[dlopen])
AC_CHECK_LIB([crypto],[MD5])
AX_PKG_SWIG(2.0.0, [ has_swig=true ], [ has_swig=false  ])
AX_SWIG_ENABLE_CXX
AX_SWIG_MULTI_MODULE_SUPPORT
AX_SWIG_PYTHON
AM_CONDITIONAL(HAS_SWIG, test "$has_swig" = "true" )



#
# Since the readline library on Mac is often not the GNU one for which
# the calculator is setup, we disable its use by default. If the user has made 
# the necessary arrangements for the GNU readline to be used instead of the 
# one that is on the system, then she/he can enable the use of the library
# explicitely using --enable-readline=yes. 
#
host_os=`uname`
case $host_os in
     darwin*|Darwin*) 
     		      AC_MSG_RESULT(yes)
		      use_readline_default=false ;;
     *) AC_MSG_RESULT(no)
     	use_readline_default=true ;;
esac

AC_MSG_CHECKING(whether svnversion is present)

svnpath=`which svnversion` 
has_svnversion=true;
if test "x$svnpath" == "x"  ; then has_svnversion=false; fi
case $has_svnversion in
     true) 
      AC_MSG_RESULT(yes) ;;
     false) 
     AC_MSG_RESULT(no) ;; 
esac
AM_CONDITIONAL(HAS_SVNVERSION, test "$has_svnversion" = "true" )



 

AC_ARG_ENABLE(readline,
[  --enable-readline    uses the readline library for the input of the calculator],
[case "${enableval}" in
  yes) readline=true ;;
  no)  readline=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-readline) ;;
esac],[readline=$use_readline_default])
AC_CHECK_HEADERS([readline/readline.h],has_readline=true,has_readline=false)
AM_CONDITIONAL(USE_READLINE, test x$has_readline = xtrue && test x$readline = xtrue  )

AC_ARG_ENABLE(sherpaplugin,
  AC_HELP_STRING([--enable-sherpaplugin=/path/to/sherpa], [Enable Sherpa plugin.]),
  [ AC_MSG_CHECKING(for Sherpa installation directory);
    case "${enableval}" in
      no)   AC_MSG_RESULT(Sherpa not enabled); sherpaplugin=false ;;
      yes)  if test -x "`which Sherpa-config`"; then
              CONDITIONAL_SHERPAINCS="-I$(Sherpa-config --incdir)";
            else
              AC_MSG_ERROR(Sherpa is not in your path. Please provide the installation directory.);
            fi;
            AC_MSG_RESULT([${enableval}]); sherpaplugin=true;;
      *)    if test -x "${enableval}/bin/Sherpa-config"; then
              CONDITIONAL_SHERPAINCS="-I$(${enableval}/bin/Sherpa-config --incdir)";
            else
              AC_MSG_ERROR(${enableval} is not a valid path.);
            fi;
            AC_MSG_RESULT([${enableval}]); sherpaplugin=true;;
    esac
    ],
  [ sherpaplugin=false ]
)
AC_SUBST(CONDITIONAL_SHERPAINCS)
AM_CONDITIONAL([SHERPAPLUGIN], [test x$sherpaplugin = xtrue])

AC_ARG_ENABLE(pythoninterface,
[  --enable-pythoninterface    compiles the python interface],
[case "${enableval}" in
  yes) pythoninterface=true ;;
  no)  pythoninterface=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-pythoninterface) ;;
esac],[pythoninterface=false])
AM_CONDITIONAL([PYTHONINTERFACE], [test x$pythoninterface = xtrue])

AC_ARG_ENABLE(LHpythoninterface,
[  --enable-LHpythoninterface    compiles the python LH interface],
[case "${enableval}" in
  yes) LHpythoninterface=true ;;
  no)  LHpythoninterface=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-LHpythoninterface) ;;
esac],[LHpythoninterface=false])
AM_CONDITIONAL([BHLHPY], [test x$LHpythoninterface = xtrue])


if test x$pythoninterface = xtrue || test x$LHpythoninterface = xtrue ; then
    PYTHON_INCLUDE=`python-config --includes `
fi

AC_SUBST([PYTHON_INCLUDE])


AC_ARG_ENABLE(public,
[  --enable-public    compiles the public BHversion],
[case "${enableval}" in
  yes) public=true ;;
  no)  public=false ;;
  *) AC_MSG_ERROR(bad value ${enableval} for --enable-public) ;;
esac],[public=true])
AM_CONDITIONAL([PUBLIC], [test x$public = xtrue])

AM_CPPFLAGS="  -std=c++98 -fpermissive  -I\${top_srcdir} -I\${top_srcdir}/src "

if test x$public = xtrue ; then
    AM_CPPFLAGS="$AM_CPPFLAGS -DBH_PUBLIC "
else
    AM_CPPFLAGS="$AM_CPPFLAGS  "
fi


AC_CONFIG_FILES([
    Makefile
    src/Makefile
    src/rational_eval/Makefile
    src/ratext/Makefile
    src/cut/Makefile
    src/cut_eval/Makefile
    src/Interface/Makefile
    src/trees_eval/Makefile
    test/Makefile
    test/compare_mpmpmp.grace
    test/testAll.py
    test/CollTest.py
    test/compare_mmpmpp.grace
    test/compare_mmpmmp.grace
    my_programs/Makefile
    utils/Makefile
    FeynDiagram/Makefile
    blackhat-config
    dataInstall
    src/BHpath.h
    SherpaPlugin/Makefile
	datafiles/Makefile
	datafiles/cut/Makefile
	datafiles/rat/Makefile
	datafiles/parents/Makefile
	datafiles/assembly/Makefile
	datafiles/trees/Makefile
    Python/Makefile
    examples/Makefile
])



AC_ARG_ENABLE([BHdebug],
     [  --enable-BHdebug    Turn on debugging],
     [case "${enableval}" in
       yes) BHdebug=true ;;
       no)  BHdebug=false ;;
       *) AC_MSG_ERROR([bad value ${enableval} for --enable-BHdebug]) ;;
     esac],[BHdebug=false])
AM_CONDITIONAL([BHDEBUG], [test x$BHdebug = xtrue])


if test x$BHdebug = xtrue ; then
    AM_CPPFLAGS="$AM_CPPFLAGS -DBH_DEBUG_ON "
else
    AM_CPPFLAGS="$AM_CPPFLAGS  "
fi


AC_ARG_ENABLE([BHtiming],
     [  --enable-BHtiming    Turn on time monitoring],
     [case "${enableval}" in
       yes) BHtiming=true ;;
       no)  BHtiming=false ;;
       *) AC_MSG_ERROR([bad value ${enableval} for --enable-BHtiming]) ;;
     esac],[BHtiming=false])

if test x$BHtiming = xtrue ; then
    if test x$public = xtrue ; then
    	BHtiming=false
    	AC_MSG_WARN([Option BHtiming not supported in the public version])
	else
    	AM_CPPFLAGS="$AM_CPPFLAGS -DBH_TIMING_ON "
	fi
fi
AM_CONDITIONAL([BHTIMING], [test x$BHtiming = xtrue])


AC_ARG_ENABLE([useGMP],
     [  --enable-useGMP    Turns on use of GMP library],
     [AC_MSG_CHECKING(whether to use the GMP library)
     case "${enableval}" in
       yes)AC_MSG_RESULT(yes); useGMP=true ;;
       no)  AC_MSG_RESULT(no);useGMP=false ;;
       *) AC_MSG_ERROR([bad value ${enableval} for --enable-useGMP]) ;;
     esac],[useGMP=false])

if test x$useGMP = xtrue ; then
    if test x$public = xtrue ; then
    	useGMP=false
    	AC_MSG_WARN([Option useGMP not supported in the public version])
	else
    	AM_CPPFLAGS="$AM_CPPFLAGS -DBH_USE_GMP "
AC_CHECK_LIB([mpfr],[mpfr_get_d])
AC_CHECK_LIB([gmp],[get_nbr_digits])
AC_CHECK_LIB([gmpxx],[_init])
	fi


fi
AM_CONDITIONAL([BH_USE_GMP], [test x$useGMP = xtrue])



AC_ARG_ENABLE([useOMP],
     [  --enable-useOMP    Turns on use of openMP library],
     [AC_MSG_CHECKING(whether to use the openMP library)
     case "${enableval}" in
       yes)AC_MSG_RESULT(yes); useOMP=true ;;
       no)  AC_MSG_RESULT(no);useOMP=false ;;
       *) AC_MSG_ERROR([bad value ${enableval} for --enable-useOMP]) ;;
     esac],[useOMP=false])

if test x$useOMP = xtrue ; then
    if test x$public = xtrue ; then
    	useOMP=false
    	AC_MSG_WARN([Option useOMP not supported in the public version])
	else
    	AM_CPPFLAGS="$AM_CPPFLAGS -DBH_USE_OMP "
    	AM_CXXFLAGS="$AM_CXXFLAGS -fopenmp "
	fi
fi

AM_CONDITIONAL([BH_USE_OMP], [test x$useOMP = xtrue])


# introduce the optional configure parameter for a non-standard install prefix of GMP
AC_ARG_WITH([GMPpath],
    [AS_HELP_STRING([--with-GMPpath=prefix],
        [try this for a non-standard install prefix of the GMP library])],
    [
    if test x$public = xtrue ; then
    	GMPPATHSET=false
    	AC_MSG_WARN([Option with-GMPpath not supported in the public version])
	else
    	GMPPATHSET=true
	fi
    ],
    [GMPPATHSET=false])
AM_CONDITIONAL([GMP_PATH_SET], [test x$GMPPATHSET = xtrue])
	
if test x$GMPPATHSET = xtrue ; then
    AM_CXXFLAGS="$AM_CXXFLAGS -I${with_GMPpath}/include"
    AM_LDFLAGS="$AM_LDFLAGS -L${with_GMPpath}/lib"
    AM_GMP_PATH="$with_GMPpath"
fi




# introduce the optional configure parameter for a non-standard install prefix of qd
AC_ARG_WITH([QDpath],
    [AS_HELP_STRING([--with-QDpath=prefix],
        [use this for a non-standard install prefix of the QD library])],
    [QDPATHSET=true],
    [QDPATHSET=false])
AM_CONDITIONAL([QD_PATH_SET], [test x$QDPATHSET = xtrue])
	
if test x$QDPATHSET = xtrue ; then
	if test -x "${with_QDpath}/bin/qd-config" ; then
    	AM_CXXFLAGS="$AM_CXXFLAGS -I${with_QDpath}/include"
    	AM_LDFLAGS="$AM_LDFLAGS -L${with_QDpath}/lib -lqd"
    	AM_QD_PATH="$with_QDpath"
	else
  		AC_MSG_ERROR(["no qd-config found for the path you provided! ($with_QDpath) "])
	fi
else 
   	if which qd-config ; then
    	AM_CXXFLAGS="$AM_CXXFLAGS $(qd-config --cflags)"
    	AM_LDFLAGS="$AM_LDFLAGS $(qd-config --libs)"
    	AM_QD_PATH="$(qd-config --prefix)"
	else
  		AC_MSG_ERROR([no qd-config found in PATH! Are you sure the qd library is installed? Consider using the --with-QDpath option.])
	fi
fi



AC_SUBST([AM_CXXFLAGS])
AC_SUBST([AM_CPPFLAGS])
AC_SUBST([AM_LDFLAGS])
AC_SUBST([AM_GMP_PATH])
AC_SUBST([AM_QD_PATH])

	
AC_PROG_LIBTOOL
#AC_CONFIG_SUBDIRS([qd])
AC_OUTPUT

#chmod +x blackhat-config
echo " --- BlackHat Configuration: "
echo "  has the readline library    = $has_readline"
echo "  use libreadline             = $readline"
echo "  has svnversion              = $has_svnversion"
echo "  use GMP                     = $useGMP"
echo "  use openMP                  = $useOMP"
echo "  build public version        = $public"


if test x$GMPPATHSET == xtrue ; then echo "  path for GMP                = $with_GMPpath"; fi
if test x$QDPATHSET == xtrue ; then echo "  path for QD                 = $with_QDpath"; fi
if test x$pythoninterface == xtrue ; then echo "  generating pythoninterface   "; fi
