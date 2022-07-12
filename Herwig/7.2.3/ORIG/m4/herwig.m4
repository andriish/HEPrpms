dnl ##### THEPEG #####
AC_DEFUN([HERWIG_CHECK_THEPEG],
[
defaultlocation="${prefix}"
test "x$defaultlocation" = xNONE && defaultlocation="${ac_default_prefix}"
AC_MSG_CHECKING([for libThePEG in])
AC_ARG_WITH(thepeg,
        AC_HELP_STRING([--with-thepeg=DIR],[location of ThePEG installation]),
        [],
	[with_thepeg="${defaultlocation}"])
AC_MSG_RESULT([$with_thepeg])

if test "x$with_thepeg" = "xno"; then
	AC_MSG_ERROR([Cannot build Herwig without ThePEG. Please set --with-thepeg.])
fi

THEPEGLDFLAGS="-L${with_thepeg}/lib/ThePEG"

THEPEGHASLHAPDF="no"
if test -e ${with_thepeg}/lib/ThePEG/ThePEGLHAPDF.so ; then
   THEPEGHASLHAPDF="yes"
fi
if test "${host_cpu}" == "x86_64" -a -e ${with_thepeg}/lib64/ThePEG/libThePEG.so ; then
  THEPEGLDFLAGS="-L${with_thepeg}/lib64/ThePEG"
  if test -e ${with_thepeg}/lib64/ThePEG/ThePEGLHAPDF.so ; then
      THEPEGHASLHAPDF="yes"
  fi
fi

if test "x$THEPEGHASLHAPDF" == "xno" ; then
   AC_MSG_ERROR([Herwig requires ThePEG to be build with lhapdf.])
fi

THEPEGHASFASTJET="no"
if test -e ${with_thepeg}/lib/ThePEG/FastJetFinder.so ; then
   THEPEGHASFASTJET="yes"
fi
if test "${host_cpu}" == "x86_64" -a -e ${with_thepeg}/lib64/ThePEG/libThePEG.so ; then
  THEPEGLDFLAGS="-L${with_thepeg}/lib64/ThePEG"
  if test -e ${with_thepeg}/lib64/ThePEG/FastJetFinder.so ; then
      THEPEGHASFASTJET="yes"
  fi
fi

if test "x$THEPEGHASFASTJET" == "xno" ; then
   AC_MSG_ERROR([Herwig requires ThePEG to be build with FastJet.])
fi

THEPEGPATH="${with_thepeg}"

oldldflags="$LDFLAGS"
oldlibs="$LIBS"

LDFLAGS="$LDFLAGS $THEPEGLDFLAGS"
AC_CHECK_LIB([ThePEG],[debugThePEG],[],
	[AC_MSG_ERROR([No ThePEG libraries in $THEPEGLDFLAGS. Please set --with-thepeg.])])

AC_SUBST([THEPEGLIB],[-lThePEG])
AC_SUBST(THEPEGLDFLAGS)
AC_SUBST(THEPEGPATH)
AC_SUBST(THEPEGHASLHAPDF)
AC_SUBST(THEPEGHASFASTJET)

LIBS="$oldlibs"
LDFLAGS="$oldldflags"

AC_MSG_CHECKING([for ThePEG headers in])
AC_ARG_WITH([thepeg-headers],
        AC_HELP_STRING([--with-thepeg-headers=DIR],[location of ThePEG include directory]),
        [],
	[with_thepeg_headers="${with_thepeg}/include"])
AC_MSG_RESULT([$with_thepeg_headers])

if test "x$with_thepeg_headers" = "xno"; then
	AC_MSG_ERROR([Cannot build Herwig without ThePEG headers. Please set --with-thepeg-headers.])
fi

THEPEGINCLUDE="-I$with_thepeg_headers"

oldcppflags="$CPPFLAGS"
CPPFLAGS="$CPPFLAGS $THEPEGINCLUDE"
AC_CHECK_HEADER([ThePEG/Config/ThePEG.h],[],
	[AC_MSG_ERROR([No ThePEG headers in $with_thepeg_headers. Please set --with-thepeg-headers.])])
CPPFLAGS="$oldcppflags"

AC_SUBST(THEPEGINCLUDE)

AC_MSG_CHECKING([for HepMCAnalysis.so in ThePEG])

THEPEGHASHEPMC="no"
if test -e ${with_thepeg}/lib/ThePEG/HepMCAnalysis.so ; then
   THEPEGHASHEPMC="yes"
fi
if test "${host_cpu}" == "x86_64" -a -e ${with_thepeg}/lib64/ThePEG/libThePEG.so ; then
  THEPEGLDFLAGS="-L${with_thepeg}/lib64/ThePEG"
  if test -e ${with_thepeg}/lib64/ThePEG/HepMCAnalysis.so ; then
    THEPEGHASHEPMC="yes"
  fi
fi

if test "x$THEPEGHASHEPMC" == "xno" ; then
  CREATE_HEPMC="# create"
  AC_MSG_RESULT([not found])
else
  CREATE_HEPMC="create"
  AC_MSG_RESULT([found])
fi

AC_SUBST([CREATE_HEPMC])

AC_MSG_CHECKING([for RivetAnalysis.so in ThePEG])

THEPEGHASRIVET="no"
if test -e ${with_thepeg}/lib/ThePEG/RivetAnalysis.so ; then
   THEPEGHASRIVET="yes"
fi
if test "${host_cpu}" == "x86_64" -a -e ${with_thepeg}/lib64/ThePEG/libThePEG.so ; then
  THEPEGLDFLAGS="-L${with_thepeg}/lib64/ThePEG"
  if test -e ${with_thepeg}/lib64/ThePEG/RivetAnalysis.so ; then
    THEPEGHASRIVET="yes"
  fi
fi

if test "x$THEPEGHASRIVET" == "xno" ; then
  CREATE_RIVET="# create"
  AC_MSG_RESULT([not found])
else
  CREATE_RIVET="create"
  AC_MSG_RESULT([found])
fi

AM_CONDITIONAL(HAVE_RIVET,[test x$THEPEGHASRIVET = xyes])
AC_SUBST([CREATE_RIVET])
])

dnl ##### LOOPTOOLS #####
AC_DEFUN([HERWIG_LOOPTOOLS],
[
AC_REQUIRE([AC_PROG_FC])
AC_REQUIRE([AC_FC_LIBRARY_LDFLAGS])
AC_REQUIRE([AC_PROG_CC])
AC_REQUIRE([HERWIG_COMPILERFLAGS])

AC_MSG_CHECKING([if Looptools build works])
enable_looptools=yes

if test "x$GCC" = "xyes"; then
   case "${host}" in
      x86_64-*|*-darwin1*)
	AM_FCFLAGS="$AM_FCFLAGS -fdefault-integer-8"
      	;;
   esac

   AC_LANG_PUSH([Fortran])
   	oldFCFLAGS="$FCFLAGS"
   	FCFLAGS="$AM_FCFLAGS"
   	AC_COMPILE_IFELSE(
	   	AC_LANG_PROGRAM([],[      print *[,]"Hello"]),
		[],
		[AC_MSG_RESULT([no])
 		 AC_MSG_ERROR([needs gfortran on 64bit machines])]
	)
	FCFLAGS="$oldFCFLAGS"
  AC_LANG_POP([Fortran])
fi
AC_MSG_RESULT([$enable_looptools])

AC_LANG_PUSH([Fortran])
AC_MSG_CHECKING([checking if fortran compiler compiles argument mismatches])
AC_COMPILE_IFELSE(AC_LANG_SOURCE([[
      program temp
      call a(1.0D0)
      end program
      subroutine a(b)
      double complex b
      end]]),
      [AC_MSG_RESULT([yes])],
      [oldFCFLAGS="$FCFLAGS"
      FCFLAGS="-std=legacy"
      AC_MSG_CHECKING([checking if fortran compiler compiles argument mismatches with -std=legacy])
      AC_COMPILE_IFELSE(AC_LANG_SOURCE([[
      program temp
      double precision b
      double complex c
      b = 1.0D0
      c  =1.0D0
      call a(b)
      call a(c)
      end program
      subroutine a(b)
      double complex b
      end]]),
      [AC_MSG_RESULT([yes])
       AM_FCFLAGS="$AM_FCFLAGS -std=legacy"],
      [AC_MSG_RESULT([no])
      AC_MSG_ERROR([fortran compiler won't compile LoopTools])])
     FCFLAGS="$oldFCFLAGS -std=legacy"]
)

AC_MSG_CHECKING([checking if fortran compiler compiles long lines])
AC_COMPILE_IFELSE(AC_LANG_SOURCE([[
       program temp
       write (*,*) 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
       end program]]),
       [AC_MSG_RESULT([yes])],
       [AC_MSG_RESULT([no])
       oldFCFLAGS="$FCFLAGS"
       FCFLAGS="-ffixed-line-length-none"
       AC_MSG_CHECKING([checking if fortran compiler compiles long lines with -ffixed-line-length-none])
       AC_COMPILE_IFELSE(AC_LANG_SOURCE([[
       program temp
       write (*,*) 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
       end program]]),
       [AC_MSG_RESULT([yes])
       AM_FCFLAGS="$AM_FCFLAGS -ffixed-line-length-none"
       FCFLAGS="$oldFCFLAGS -ffixed-line-length-none"],
       [FCFLAGS="-132"
       AC_MSG_CHECKING([checking if fortran compiler compiles long lines with -132])
       AC_COMPILE_IFELSE(AC_LANG_SOURCE([[
       program temp
       write (*,*) 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
       end program]]),
       [AC_MSG_RESULT([yes])
       AM_FCFLAGS="$AM_FCFLAGS -132"
       FCFLAGS="$oldFCFLAGS -132"],
       [AC_MSG_RESULT([no])
       AC_MSG_ERROR([fortran compiler won't compile LoopTools])])])
      ]
)





AC_LANG_POP([Fortran])


AC_SUBST([F77],[$FC])
AC_SUBST([FFLAGS],[$FCFLAGS])
AC_SUBST([AM_FFLAGS],[$AM_FCFLAGS])
AC_SUBST([FLIBS],[$FCLIBS])
])

dnl ##### VBFNLO #####
AC_DEFUN([HERWIG_CHECK_VBFNLO],
[
AC_MSG_CHECKING([for VBFNLO])

AC_ARG_WITH([vbfnlo],
    AS_HELP_STRING([--with-vbfnlo=DIR], [Installation path of VBFNLO]),
    [],
    [with_vbfnlo=no]

)

AC_MSG_RESULT([$with_vbfnlo])

AS_IF([test "x$with_vbfnlo" != "xno"],
      [AC_CHECK_FILES(
      ${with_vbfnlo}/lib/VBFNLO/libVBFNLO.so,
      [have_vbfnlo=lib], [have_vbfnlo=no])],
      [have_vbfnlo=no])

AS_IF([test "x$with_vbfnlo" != "xno" -a "x$have_vbfnlo" = "xno" ],
      [AC_CHECK_FILES(
      ${with_vbfnlo}/lib64/VBFNLO/libVBFNLO.so,
      [have_vbfnlo=lib64], [have_vbfnlo=no])])

AS_IF([test "x$with_vbfnlo" != "xno" -a "x$have_vbfnlo" = "xno" ],
      [AC_CHECK_FILES(
      ${with_vbfnlo}/lib/VBFNLO/libVBFNLO.dylib,
      [have_vbfnlo=lib], [have_vbfnlo=no])])

AS_IF([test "x$with_vbfnlo" != "xno" -a "x$have_vbfnlo" = "xno" ],
      [AC_CHECK_FILES(
      ${with_vbfnlo}/lib64/VBFNLO/libVBFNLO.dylib,
      [have_vbfnlo=lib64], [have_vbfnlo=no])])

AS_IF([test "x$have_vbfnlo" = "xlib"],
      [VBFNLOLIBS=${with_vbfnlo}/lib/VBFNLO
      AC_SUBST(VBFNLOLIBS)
      ])

AS_IF([test "x$have_vbfnlo" = "xlib64"],
      [VBFNLOLIBS=${with_vbfnlo}/lib64/VBFNLO
      AC_SUBST(VBFNLOLIBS)
      ])

AS_IF([test "x$with_vbfnlo" != "xno" -a "x$have_vbfnlo" = "xno"],
      [AC_MSG_ERROR([vbfnlo requested but not found])])

AM_CONDITIONAL(HAVE_VBFNLO,[test "x$have_vbfnlo" = "xlib" -o "x$have_vbfnlo" = "xlib64"])

if test "x$have_vbfnlo" = "xlib" -o "x$have_vbfnlo" = "xlib64" ; then
        AC_REQUIRE([AC_PROG_SED])
        VBFNLOINCLUDE=${with_vbfnlo}/include
	AC_SUBST(VBFNLOINCLUDE)
        VBFNLOLIB=$(echo ${with_vbfnlo}/${have_vbfnlo}/VBFNLO | $SED -e 's%/\+%/%g')
        AC_SUBST(VBFNLOLIB)
     	LOAD_VBFNLO="library"
     	CREATE_VBFNLO="create"
     	INSERT_VBFNLO="insert"
     	SET_VBFNLO="set"
     	DO_VBFNLO="do"
     	MKDIR_VBFNLO="mkdir"
else
     	LOAD_VBFNLO="# library"
	CREATE_VBFNLO="# create"
     	INSERT_VBFNLO="# insert"
     	SET_VBFNLO="# set"
     	DO_VBFNLO="# do"
     	MKDIR_VBFNLO="# mkdir"
fi

AC_SUBST([LOAD_VBFNLO])
AC_SUBST([CREATE_VBFNLO])
AC_SUBST([INSERT_VBFNLO])
AC_SUBST([SET_VBFNLO])
AC_SUBST([DO_VBFNLO])
AC_SUBST([MKDIR_VBFNLO])

])

dnl ##### njet #####
AC_DEFUN([HERWIG_CHECK_NJET],
[
AC_MSG_CHECKING([for njet])

AC_ARG_WITH([njet],
    AS_HELP_STRING([--with-njet=DIR], [Installation path of njet]),
    [],
    [with_njet=no]

)

AC_MSG_RESULT([$with_njet])

AS_IF([test "x$with_njet" != "xno"],
      [AC_CHECK_FILES(
      ${with_njet}/lib/libnjet2.so,
      [have_njet=lib], [have_njet=no])],
      [have_njet=no])

AS_IF([test "x$with_njet" != "xno" -a "x$have_njet" = "xno" ],
      [AC_CHECK_FILES(
      ${with_njet}/lib64/libnjet2.so,
      [have_njet=lib64], [have_njet=no])])

AS_IF([test "x$with_njet" != "xno" -a "x$have_njet" = "xno" ],
      [AC_CHECK_FILES(
      ${with_njet}/lib/libnjet2.dylib,
      [have_njet=lib], [have_njet=no])])

AS_IF([test "x$have_njet" = "xlib"],
      [NJETLIBPATH=${with_njet}/lib
      AC_SUBST(NJETLIBPATH)
      NJETINCLUDEPATH=${with_njet}/include
      AC_SUBST(NJETINCLUDEPATH)
      NJETPREFIX=${with_njet}
      AC_SUBST(NJETPREFIX)
      ])

AS_IF([test "x$have_njet" = "xlib64"],
      [NJETLIBPATH=${with_njet}/lib64
      AC_SUBST(NJETLIBPATH)
      NJETINCLUDEPATH=${with_njet}/include
      AC_SUBST(NJETINCLUDEPATH)
      NJETPREFIX=${with_njet}
      AC_SUBST(NJETPREFIX)
      ])

AS_IF([test "x$with_njet" != "xno"  -a "x$have_njet" = "xno"],
      [AC_MSG_ERROR([njet requested but not found])])

AM_CONDITIONAL(HAVE_NJET,[test "x$have_njet" = "xlib" -o "x$have_njet" = "xlib64"])

AS_IF([test "x$with_njet" != "xno"],[AC_MSG_CHECKING([for Njet version])
tmp_njetversion=[$(${with_njet}/bin/njet.py --version 2>&1 | grep -oE '[0-9]{4}$')]
AS_IF([test -z "$tmp_njetversion"],
      [tmp_njetversion=1023])
      AC_MSG_RESULT([${tmp_njetversion}])
      NJET_VERSION=${tmp_njetversion}
      AC_SUBST(NJET_VERSION)
])

if test "x$have_njet" = "xlib" -o "x$have_njet" = "xlib64" ; then
     	LOAD_NJET="library"
     	CREATE_NJET="create"
     	INSERT_NJET="insert"
     	DO_NJET="do"
else
     	LOAD_NJET="# library"
      CREATE_NJET="# create"
     	INSERT_NJET="# insert"
     	DO_NJET="# do"
fi

AC_SUBST([LOAD_NJET])
AC_SUBST([CREATE_NJET])
AC_SUBST([INSERT_NJET])
AC_SUBST([DO_NJET])

])



dnl ##### gosam #####
AC_DEFUN([HERWIG_CHECK_GOSAM],
[
AC_MSG_CHECKING([for GoSam])

AC_ARG_WITH([gosam],
    AS_HELP_STRING([--with-gosam=DIR], [Installation path of GoSam]),
    [],
    [with_gosam=no]
)

AC_MSG_RESULT([$with_gosam])

AS_IF([test "x$with_gosam" != "xno"],
      [AC_CHECK_FILES(
      ${with_gosam}/bin/gosam.py,
      [have_gosam=lib], [have_gosam=no])],
      [have_gosam=no])

AS_IF([test "x$have_gosam" = "xlib"],
      [GOSAMPREFIX=${with_gosam}
      AC_SUBST(GOSAMPREFIX)
      ])

AS_IF([test "x$with_gosam" != "xno"  -a "x$have_gosam" = "xno"],
      [AC_MSG_ERROR([GoSam requested but not found])])

AS_IF([test "x$with_gosam" != "xno"],
[AC_MSG_CHECKING([for GoSam version >= 2.0.4])
tmp_gosamversion=[$(${with_gosam}/bin/gosam.py --version | grep 'GoSam.*rev' | cut -d' ' -f2)]
AX_COMPARE_VERSION([${tmp_gosamversion}],[lt],[2.0.4],
                   [AC_MSG_RESULT([no])
                    AC_MSG_ERROR([Herwig requires GoSam 2.0.4 or later, found ${tmp_gosamversion}])],
                   [AC_MSG_RESULT([yes])])])

AM_CONDITIONAL(HAVE_GOSAM,[test "x$have_gosam" = "xlib" ])

if test "x$have_gosam" = "xlib"  ; then
     	LOAD_GOSAM="library"
     	CREATE_GOSAM="create"
     	INSERT_GOSAM="insert"
     	DO_GOSAM="do"
else
     	LOAD_GOSAM="# library"
	CREATE_GOSAM="# create"
     	INSERT_GOSAM="# insert"
     	DO_GOSAM="# do"
fi

AC_SUBST([LOAD_GOSAM])
AC_SUBST([CREATE_GOSAM])
AC_SUBST([INSERT_GOSAM])
AC_SUBST([DO_GOSAM])


])


dnl ##### gosam-contrib #####
AC_DEFUN([HERWIG_CHECK_GOSAM_CONTRIB],
[
AC_MSG_CHECKING([for gosam-contrib])

AC_ARG_WITH([gosam-contrib],
    AS_HELP_STRING([--with-gosam-contrib=DIR], [Installation path of gosam-contrib]),
    [],
    [with_gosam_contrib=no]
)

AC_MSG_RESULT([$with_gosam_contrib])

AS_IF([test "x$with_gosam_contrib" = "xno" -a "x$with_gosam" != "xno"],
      [AC_CHECK_FILES(
      ${with_gosam}/lib/libsamurai.so,
      [with_gosam_contrib=${with_gosam}], [])
])

AS_IF([test "x$with_gosam_contrib" = "xno" -a "x$with_gosam" != "xno"],
      [AC_CHECK_FILES(
      ${with_gosam}/lib64/libsamurai.so,
      [with_gosam_contrib=${with_gosam}], [])
])

AS_IF([test "x$with_gosam_contrib" = "xno" -a "x$with_gosam" != "xno"],
      [AC_CHECK_FILES(
      ${with_gosam}/lib/libsamurai.dylib,
      [with_gosam_contrib=${with_gosam}], [])
])

AS_IF([test "x$with_gosam_contrib" = "xno" -a "x$with_gosam" != "xno"],
      [AC_CHECK_FILES(
      ${with_gosam}/lib64/libsamurai.dylib,
      [with_gosam_contrib=${with_gosam}], [])
])

AS_IF([test "x$with_gosam_contrib" = "xno" -a "x$with_gosam" != "xno"],
      [AC_MSG_ERROR([GoSam requested without requesting GoSam-Contrib])])

AS_IF([test "x$with_gosam_contrib" != "xno"],
      [AC_CHECK_FILES(
      ${with_gosam_contrib}/lib/libsamurai.so,
      [have_gosam_contrib=lib], [have_gosam_contrib=no])],
      [have_gosam_contrib=no])

AS_IF([test "x$with_gosam_contrib" != "xno" -a "x$have_gosam_contrib" = "xno" ],
      [AC_CHECK_FILES(
      ${with_gosam_contrib}/lib64/libsamurai.so,
      [have_gosam_contrib=lib64], [have_gosam_contrib=no])])

AS_IF([test "x$with_gosam_contrib" != "xno" -a "x$have_gosam_contrib" = "xno" ],
      [AC_CHECK_FILES(
      ${with_gosam_contrib}/lib/libsamurai.dylib,
      [have_gosam_contrib=lib], [have_gosam_contrib=no])])

AS_IF([test "x$with_gosam_contrib" != "xno" -a "x$have_gosam_contrib" = "xno" ],
      [AC_CHECK_FILES(
      ${with_gosam_contrib}/lib64/libsamurai.dylib,
      [have_gosam_contrib=lib64], [have_gosam_contrib=no])])







AS_IF([test "x$have_gosam_contrib" != "xno"],
      [GOSAMCONTRIBPREFIX=${with_gosam_contrib}
      AC_SUBST(GOSAMCONTRIBPREFIX)
      ])

AS_IF([test "x$have_gosam_contrib" = "xlib"],
      [GOSAMCONTRIBLIBS=${with_gosam_contrib}/lib
      AC_SUBST(GOSAMCONTRIBLIBS)
      ])

AS_IF([test "x$have_gosam_contrib" = "xlib64"],
      [GOSAMCONTRIBLIBS=${with_gosam_contrib}/lib64
      AC_SUBST(GOSAMCONTRIBLIBS)
      ])

AS_IF([test "x$with_gosam_contrib" != "xno"  -a "x$have_gosam_contrib" = "xno"],
      [AC_MSG_ERROR([GoSam-Contrib requested but not found])])

AM_CONDITIONAL(HAVE_GOSAM_CONTRIB,[test "x$have_gosam_contrib" = "xlib" -o "x$have_gosam_contrib" = "xlib64"])

if test "x$have_gosam_contrib" = "xlib" -o "x$have_gosam_contrib" = "xlib64" ; then
        LOAD_GOSAM_CONTRIB="library"
        CREATE_GOSAM_CONTRIB="create"
        INSERT_GOSAM_CONTRIB="insert"
else
        LOAD_GOSAM_CONTRIB="# library"
        CREATE_GOSAM_CONTRIB="# create"
        INSERT_GOSAM_CONTRIB="# insert"
fi

AC_SUBST([LOAD_GOSAM_CONTRIB])
AC_SUBST([CREATE_GOSAM_CONTRIB])
AC_SUBST([INSERT_GOSAM_CONTRIB])


])


dnl ##### OpenLoops #####
AC_DEFUN([HERWIG_CHECK_OPENLOOPS],
[
AC_MSG_CHECKING([for OpenLoops])

AC_ARG_WITH([openloops],
    AS_HELP_STRING([--with-openloops=DIR], [Installation path of OpenLoops]),
    [],
    [with_openloops=no]

)

AC_MSG_RESULT([$with_openloops])

AS_IF([test "x$with_openloops" != "xno"],
      [AC_CHECK_FILES(
      ${with_openloops}/lib/libopenloops.so,
      [have_openloops=lib], [have_openloops=no])],
      [have_openloops=no])

AS_IF([test "x$with_openloops" != "xno" -a "x$have_openloops" = "xno" ],
      [AC_CHECK_FILES(
      ${with_openloops}/lib/libopenloops.dylib,
      [have_openloops=lib], [have_openloops=no])])

AS_IF([test "x$with_openloops" != "xno" -a "x$have_openloops" = "xno" ],
      [AC_CHECK_FILES(
      ${with_openloops}/lib64/libopenloops.so,
      [have_openloops=lib64], [have_openloops=no])])


AS_IF([test "x$with_openloops" != "xno" -a "x$have_openloops" = "xno" ],
      [AC_CHECK_FILES(
      ${with_openloops}/lib64/libopenloops.dylib,
      [have_openloops=lib64], [have_openloops=no])])





AS_IF([test "x$have_openloops" = "xlib"],
      [OPENLOOPSLIBS=${with_openloops}/lib
      AC_SUBST(OPENLOOPSLIBS)
      ])

AS_IF([test "x$have_openloops" = "xlib64"],
      [OPENLOOPSLIBS=${with_openloops}/lib64
      AC_SUBST(OPENLOOPSLIBS)
      ])

AS_IF([test "x$with_openloops" != "xno" -a "x$have_openloops" = "xno"],
      [AC_MSG_ERROR([OpenLoops requested but not found])])

AM_CONDITIONAL(HAVE_OPENLOOPS,[test "x$have_openloops" = "xlib" -o "x$have_openloops" = "xlib64"])

if test "x$have_openloops" = "xlib" -o "x$have_openloops" = "xlib64" ; then
        OPENLOOPSPREFIX=${with_openloops}
     	LOAD_OPENLOOPS="library"
     	CREATE_OPENLOOPS="create"
     	INSERT_OPENLOOPS="insert"
     	SET_OPENLOOPS="set"
     	DO_OPENLOOPS="do"
     	MKDIR_OPENLOOPS="mkdir"
else
     	LOAD_OPENLOOPS="# library"
	CREATE_OPENLOOPS="# create"
     	INSERT_OPENLOOPS="# insert"
     	SET_OPENLOOPS="# set"
     	DO_OPENLOOPS="# do"
     	MKDIR_OPENLOOPS="# mkdir"
fi

AC_SUBST([OPENLOOPSPREFIX])
AC_SUBST([LOAD_OPENLOOPS])
AC_SUBST([CREATE_OPENLOOPS])
AC_SUBST([INSERT_OPENLOOPS])
AC_SUBST([SET_OPENLOOPS])
AC_SUBST([DO_OPENLOOPS])
AC_SUBST([MKDIR_OPENLOOPS])

])

#########################################

dnl ##### madgraph #####
AC_DEFUN([HERWIG_CHECK_MADGRAPH],
[
AC_MSG_CHECKING([for MadGraph])

AC_ARG_WITH([madgraph],
    AS_HELP_STRING([--with-madgraph=DIR], [Installation path of MadGraph]),
    [],
    [with_madgraph=no]
)

AC_MSG_RESULT([$with_madgraph])

AS_IF([test "x$with_madgraph" != "xno"],
      [AC_CHECK_FILES(
      ${with_madgraph}/bin/mg5_aMC,
      [have_madgraph=yes], [have_madgraph=no])],
      [have_madgraph=no])

AS_IF([test "x$have_madgraph" = "xyes"],
      [MADGRAPHPREFIX=${with_madgraph}
      AC_SUBST(MADGRAPHPREFIX)
      ])

AS_IF([test "x$with_madgraph" != "xno"  -a "x$have_madgraph" = "xno"],
      [AC_MSG_ERROR([MadGraph requested but not found])])

AM_CONDITIONAL(HAVE_MADGRAPH,[test "x$have_madgraph" = "xyes" ])

if test "x$have_madgraph" = "xyes"  ; then
     	LOAD_MADGRAPH="library"
     	CREATE_MADGRAPH="create"
     	INSERT_MADGRAPH="insert"
     	SET_MADGRAPH="set"
     	DO_MADGRAPH="do"
else
     	LOAD_MADGRAPH="# library"
	CREATE_MADGRAPH="# create"
     	INSERT_MADGRAPH="# insert"
     	SET_MADGRAPH="# set"
     	DO_MADGRAPH="# do"
fi

AC_SUBST([LOAD_MADGRAPH])
AC_SUBST([CREATE_MADGRAPH])
AC_SUBST([INSERT_MADGRAPH])
AC_SUBST([SET_MADGRAPH])
AC_SUBST([DO_MADGRAPH])

])


AC_DEFUN([HERWIG_CHECK_PYTHIA],
[
dnl check if a directory is specified for Pythia
AC_ARG_WITH(pythia,
            [AC_HELP_STRING([--with-pythia=dir],
                            [Assume the given directory for Pythia])])

dnl search for the pythia-config script
if test "$with_pythia" = ""; then
   AC_PATH_PROG(pythiaconfig, pythia8-config, no)
else
   AC_PATH_PROG(pythiaconfig, pythia8-config, no, ${with_pythia}/bin)
fi

if test "${pythiaconfig}" = "no"; then
   AC_MSG_CHECKING(Pythia)
   AC_MSG_RESULT(no);
   PYTHIA8LIB=
#   $2
else
   PYTHIA8DATA=`${pythiaconfig} --datadir`/xmldoc
   PYTHIA8LIB="-L`${pythiaconfig} --libdir` -lpythia8"
fi

AC_SUBST(PYTHIA8DATA)
AC_SUBST(PYTHIA8LIB)

])

dnl CHECK PYTHIA END

dnl ##### EvtGen #####
AC_DEFUN([HERWIG_CHECK_EVTGEN],
[
AC_MSG_CHECKING([for evtgen])

AC_ARG_WITH([evtgen],
    AS_HELP_STRING([--with-evtgen=DIR], [Installation path of EvtGen]),
    [],
    [with_evtgen=no]
)

AC_MSG_RESULT([$with_evtgen])

AS_IF([test "x$with_evtgen" != "xno"],
      [AC_CHECK_FILES(
      ${with_evtgen}/lib/libEvtGenExternal.so,
      [have_evtgen=lib], [have_evtgen=no])],
      [have_evtgen=no])

AS_IF([test "x$with_evtgen" != "xno" -a "x$have_evtgen" = "xno"],
      [AC_CHECK_FILES(
      ${with_evtgen}/lib64/libEvtGenExternal.so,
      [have_evtgen=lib64], [have_evtgen=no])])

AS_IF([test "x$with_evtgen" != "xno" -a "x$have_evtgen" = "xno" ],
      [AC_CHECK_FILES(
      ${with_evtgen}/lib/libEvtGenExternal.dylib,
      [have_evtgen=lib], [have_evtgen=no])])

AS_IF([test "x$have_evtgen" = "xlib" -o "x$have_evtgen" = "xlib64" ],
      [EVTGENPREFIX=${with_evtgen}
      AC_SUBST(EVTGENPREFIX)
      ])

AS_IF([test "x$with_evtgen" != "xno"  -a "x$have_evtgen" = "xno"],
      [AC_MSG_ERROR([EvtGen requested but not found])])

AC_SUBST([EVTGENINCLUDE],[-I$EVTGENPREFIX/include])

AM_CONDITIONAL(HAVE_EVTGEN,[test "x$have_evtgen" = "xlib" -o "x$have_evtgen" = "xlib64"])

if test "x$have_evtgen" = "xlib"  ; then
     	LOAD_EVTGEN_DECAYS="read EvtGenBDecays.in"
     	LOAD_EVTGEN_DECAYER="read EvtGenDecayer.in"
	EVTGENLIBS="-L$with_evtgen/lib -lEvtGen -lEvtGenExternal"
elif test "x$have_evtgen" = "xlib64"  ; then
      LOAD_EVTGEN_DECAYS="read EvtGenBDecays.in"
      LOAD_EVTGEN_DECAYER="read EvtGenDecayer.in"
      EVTGENLIBS="-L$with_evtgen/lib64 -lEvtGen -lEvtGenExternal"
else
     	LOAD_EVTGEN_DECAYS="read HerwigBDecays.in"
     	LOAD_EVTGEN_DECAYER="#read EvtGenDecayer.in"
	EVTGENLIBS=""
fi

AC_SUBST([LOAD_EVTGEN_DECAYS])
AC_SUBST([LOAD_EVTGEN_DECAYER])
AC_SUBST([EVTGENLIBS])


])

dnl ###### GSL ######
AC_DEFUN([HERWIG_CHECK_GSL],
[
AC_MSG_CHECKING([for gsl location])
GSLINCLUDE=""
GSLLIBS=""

AC_ARG_WITH(gsl,
        AC_HELP_STRING([--with-gsl=DIR],[location of gsl installation @<:@default=system libs@:>@]),
        [],
	[with_gsl=system])

if test "x$with_gsl" = "xno"; then
AC_MSG_ERROR([libgsl is required. Please install the GNU scientific library and header files.])
fi

if test "x$with_gsl" = "xsystem"; then
	AC_MSG_RESULT([in system libraries])
	oldlibs="$LIBS"
	AC_CHECK_LIB(m,main)
	AC_CHECK_LIB(gslcblas,main)
	AC_CHECK_LIB(gsl,main,[],
			[
			AC_MSG_ERROR([Cannot find libgsl. Please install the GNU scientific library and header files or use --with-gsl=.])
			]
		     )
	GSLLIBS="$LIBS"
	GSLPATH="$with_gsl"
	LIBS=$oldlibs
else
	if test "`uname -m`" = "x86_64" -a -e "$with_gsl/lib64/libgsl.a" -a -d "$with_gsl/include/gsl"; then
		AC_MSG_RESULT([found in $with_gsl])
		GSLLIBS="-L$with_gsl/lib64 -R$with_gsl/lib64 -lgslcblas -lgsl"
		GSLINCLUDE="-I$with_gsl/include"
		GSLPATH="$with_gsl"
	elif test -e "$with_gsl/lib/libgsl.a" -a -d "$with_gsl/include/gsl"; then
		AC_MSG_RESULT([found in $with_gsl])
		GSLLIBS="-L$with_gsl/lib -R$with_gsl/lib -lgslcblas -lgsl"
		GSLINCLUDE="-I$with_gsl/include"
		GSLPATH="$with_gsl"
	else
		AC_MSG_RESULT([not found])
		AC_MSG_ERROR([Can't find $with_gsl/lib/libgsl.a or the headers in $with_gsl/include])
	fi
fi

AC_SUBST(GSLINCLUDE)
AC_SUBST(GSLLIBS)
AC_SUBST(GSLPATH)
])

dnl ##### COMPILERFLAGS #####
AC_DEFUN([HERWIG_COMPILERFLAGS],
[
AC_REQUIRE([HERWIG_CHECK_GSL])
AC_REQUIRE([HERWIG_CHECK_THEPEG])
AC_REQUIRE([BOOST_REQUIRE])
AC_REQUIRE([AX_COMPILER_VENDOR])

AM_CPPFLAGS="-I\$(top_builddir)/include $THEPEGINCLUDE \$(GSLINCLUDE) \$(BOOST_CPPFLAGS)"

AC_MSG_CHECKING([for debugging mode])
AC_ARG_ENABLE(debug,
        AC_HELP_STRING([--enable-debug],[debug mode, use --enable-debug=slow for additional options that slow down the run.]),
        [],
        [enable_debug=no]
        )
AC_MSG_RESULT([$enable_debug])

if test "x$enable_debug" = "xno"; then
	debugflags=""
else
	debugflags="-g"
fi

dnl -Wfloat-equal -fvisibility-inlines-hidden -Wctor-dtor-privacy -Weffc++
if test -n "$GCC"; then
	warnflags="-pedantic -Wall -Wextra -Wno-overloaded-virtual"

	if test "x$enable_debug" = "xslow"; then
		debugflags="$debugflags -fno-inline"
		AM_CPPFLAGS="$AM_CPPFLAGS -D_GLIBCXX_DEBUG"
	fi
fi

dnl do an actual capability check on ld instead of this workaround
case "${host}" in
  *-darwin*)
     ;;
  *)
     AM_LDFLAGS="-Wl,--enable-new-dtags"
     ;;
esac

case "${ax_cv_cxx_compiler_vendor}" in
     gnu)
        AM_CXXFLAGS="-pedantic -Wall -W"
        ;;
     clang)
        AM_CXXFLAGS="-pedantic -Wall -Wno-overloaded-virtual -Wno-unused-function -Wno-unused-parameter"
dnl  -Wno-unneeded-internal-declaration
        ;;
     intel)
        AM_CXXFLAGS="-strict-ansi -Wall -wd13000,1418,981,444,383,1599,1572,2259,980"
        ;;
esac



AC_SUBST(AM_CPPFLAGS)
AC_SUBST(AM_CFLAGS,  ["$warnflags $debugflags"])
AC_SUBST(AM_CXXFLAGS,["$AM_CXXFLAGS $warnflags $debugflags"])
AC_SUBST(AM_FCFLAGS,  ["$debugflags"])
AC_SUBST(AM_LDFLAGS)
])

AC_DEFUN([HERWIG_ENABLE_MODELS],
[
AC_MSG_CHECKING([if BSM models should be built])

AC_ARG_ENABLE(models,
        AC_HELP_STRING([--disable-models],[Turn off compilation of BSM models.]),
        [],
        [enable_models=yes]
        )
AC_MSG_RESULT([$enable_models])

LOAD_BSM=""
if test "$enable_models" = "yes"; then
LOAD_BSM="read BSMlibs.in"
fi
AC_SUBST(LOAD_BSM)

AM_CONDITIONAL(WANT_BSM,[test "$enable_models" = "yes"])
])

AC_DEFUN([HERWIG_OVERVIEW],
[
FCSTRING=`$FC --version | head -1`
CXXSTRING=`$CXX --version | head -1`
CCSTRING=`$CC --version | head -1`
if test "x$PYTHON" != "x:"
then
   python_was_found="yes, using Python $PYTHON_VERSION"
else
   python_was_found="no, requires Python >= 2.6"
fi

cat << _HW_EOF_ > config.herwig
*****************************************************
*** $PACKAGE_STRING configuration summary
*** Please include this information in bug reports!
***--------------------------------------------------
*** Prefix:		$prefix
***
*** BSM models:		$enable_models
*** UFO converter:	${python_was_found}
***
*** Herwig debug mode:	$enable_debug
***
*** ThePEG:		$with_thepeg
*** ThePEG headers:	$with_thepeg_headers
***
*** GoSam:		$with_gosam
*** GoSam-Contrib:      $with_gosam_contrib
*** MadGraph:        	$with_madgraph
*** njet:		$with_njet ${NJET_VERSION}
*** OpenLoops:		$with_openloops
*** VBFNLO:		$with_vbfnlo
***
*** EvtGen:		$with_evtgen
*** GSL:		$with_gsl
*** boost:              ${BOOST_CPPFLAGS:-system}
*** Fastjet:		${fjconfig}
***
*** Host:		$host
*** CC:			$CCSTRING
*** CXX:		$CXXSTRING
*** FC:			$FCSTRING
***
*** CXXFLAGS:		$CXXFLAGS
*** FCFLAGS:            $FCFLAGS
*****************************************************
_HW_EOF_
])
