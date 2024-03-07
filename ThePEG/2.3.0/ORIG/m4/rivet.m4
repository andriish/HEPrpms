dnl ##### RIVET #####
AC_DEFUN([THEPEG_CHECK_RIVET],
[
AC_REQUIRE([THEPEG_CHECK_HEPMC])
AC_REQUIRE([THEPEG_CHECK_GSL])
AC_MSG_CHECKING([for Rivet location])
RIVETINCLUDE=""
LOAD_RIVET=""
RIVETLIBS="-lRivet"

AC_ARG_WITH(rivet,
        AC_HELP_STRING([--with-rivet=DIR],[Location of Rivet installation @<:@default=system libs@:>@]),
        [],
	[with_rivet=system])

if test "x$with_hepmc" = "xno"; then
	with_rivet=no
fi
 	
if test "x$with_rivet" = "xno"; then
	AC_MSG_RESULT([Rivet support disabled.])
elif test "x$with_rivet" = "xsystem"; then
        AC_MSG_RESULT([in system libraries])
	oldlibs="$LIBS"
	LIBS="$LIBS $HEPMCLIBS"
	AC_CHECK_LIB(Rivet,main,
		[],
		[with_rivet=no
		 AC_MSG_WARN([Rivet >= 1.3 not found in system libraries])
		])
	RIVETLIBS="$LIBS"
	LIBS=$oldlibs
else
	AC_MSG_RESULT([$with_rivet])
	RIVETINCLUDE="$( $with_rivet/bin/rivet-config --cppflags )"
	RIVETLIBS="-L$with_rivet/lib -R$with_rivet/lib -lRivet"
	if test "${host_cpu}" == "x86_64" -a -e $with_rivet/lib64/libRivet.so ; then
	  RIVETLIBS="-L$with_rivet/lib64 -R$with_rivet/lib64 -lRivet"
	fi
fi

if test "x$with_rivet" != "xno"; then
        LOAD_RIVET="library RivetAnalysis.so"
	# Now lets see if the libraries work properly
	oldLIBS="$LIBS"
	oldLDFLAGS="$LDFLAGS"
	oldCPPFLAGS="$CPPFLAGS"
	LIBS="$LIBS `echo $HEPMCLIBS | sed -e 's! -R.* ! !'` `echo $RIVETLIBS | sed -e 's! -R.* ! !'` `echo $GSLLIBS | sed -e 's! -R.* ! !'`"
	LDFLAGS="$LDFLAGS"
	CPPFLAGS="$CPPFLAGS $HEPMCINCLUDE $RIVETINCLUDE $GSLINCLUDE"

	# check Rivet
	AC_MSG_CHECKING([that Rivet works])
	AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <Rivet/AnalysisHandler.hh>
]],[[Rivet::AnalysisHandler foo; foo.writeData("foo");]])],[AC_MSG_RESULT([yes])],[AC_MSG_RESULT([no]) 
	AC_MSG_RESULT([No. Use '--with-rivet=' to set a path to Rivet >= 1.3'.])
	with_rivet="no"
	LOAD_RIVET=""
	])

	LIBS="$oldLIBS"
	LDFLAGS="$oldLDFLAGS"
	CPPFLAGS="$oldCPPFLAGS"
fi

rivetversion=1
if test "x$with_rivet" = "xsystem"; then
   echo $( rivet-config --version ) | grep -q '^2\.' && rivetversion=2
   echo $( rivet-config --version ) | grep -q '^3\.' && rivetversion=3
elif test "x$with_rivet" != "xno"; then
   echo $( "$with_rivet/bin/rivet-config" --version ) | grep -q '^2\.' && rivetversion=2
   echo $( "$with_rivet/bin/rivet-config" --version ) | grep -q '^3\.' && rivetversion=3
fi

AC_DEFINE_UNQUOTED([ThePEG_RIVET_VERSION], [$rivetversion], [Rivet major version (1,2,3)])

AM_CONDITIONAL(HAVE_RIVET,[test "x$with_rivet" != "xno"])
AC_SUBST(RIVETINCLUDE)
AC_SUBST(RIVETLIBS)
AC_SUBST(LOAD_RIVET)
])
