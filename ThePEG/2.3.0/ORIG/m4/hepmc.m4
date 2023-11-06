dnl ##### HEPMC #####
AC_DEFUN([THEPEG_CHECK_HEPMC],
[
AC_MSG_CHECKING([for HepMC location])
HEPMCINCLUDE=""
HEPMCROOTIO=0

AC_ARG_WITH(hepmc,
        AC_HELP_STRING([--with-hepmc=DIR],[Location of HepMC2 or HepMC3 installation @<:@default=system libs@:>@]),
        [],
	[with_hepmc=no])

AC_ARG_WITH(hepmcversion,
        AC_HELP_STRING([--with-hepmcversion=version],[Version of HepMC]),
        [],
	[with_hepmcversion=no])

if test "x$with_hepmc" == "xno" -a "x$with_hepmcversion" != "xno"; then
   with_hepmc=yes
fi

if test "x$with_hepmcversion" == "xno"; then
   with_hepmcversion=2
fi

HEPMCLIBS=""
if test "x$with_hepmcversion" = "x2"; then
SHORTHEPMCLIBS="-lHepMC"
SHORTHEPMCLIBNAME="HepMC"
fi
if test "x$with_hepmcversion" = "x3"; then
SHORTHEPMCLIBS="-lHepMC3"
SHORTHEPMCLIBNAME="HepMC3"
fi

if test "x$with_hepmc" == "xyes"; then
    if test "x$prefix" == "xNONE"; then
      with_hepmc=$ac_default_prefix
    else
      with_hepmc=$prefix
    fi
fi

if test "x$with_hepmc" = "xno"; then
	AC_MSG_RESULT([HepMC support disabled.])
elif test "x$with_hepmc" = "xsystem"; then
        AC_MSG_RESULT([in system libraries])
	oldlibs="$LIBS"
	AC_CHECK_LIB($SHORTHEPMCLIBNAME,main,
		[],
		[with_hepmc=no
		 AC_MSG_WARN([HepMC not found in system libraries])
		])
	HEPMCLIBS="$LIBS"
	LIBS=$oldlibs
else
	AC_MSG_RESULT([$with_hepmc])
	HEPMCINCLUDE=-I$with_hepmc/include
	HEPMCLIBS="-L$with_hepmc/lib -R$with_hepmc/lib "$SHORTHEPMCLIBS
	if test "${host_cpu}" == "x86_64" -a "x$with_hepmcversion" = "x2" -a -e $with_hepmc/lib64/libHepMC.so ; then
	  HEPMCLIBS="-L$with_hepmc/lib64 -R$with_hepmc/lib64 "$SHORTHEPMCLIBS
	fi
	if test "${host_cpu}" == "x86_64" -a "x$with_hepmcversion" = "x3" -a -e $with_hepmc/lib64/libHepMC3.so ; then
	  HEPMCLIBS="-L$with_hepmc/lib64 -R$with_hepmc/lib64 "$SHORTHEPMCLIBS
	fi
fi

if test "x$with_hepmc" != "xno"; then
	# Now lets see if the libraries work properly
	oldLIBS="$LIBS"
	oldLDFLAGS="$LDFLAGS"
	oldCPPFLAGS="$CPPFLAGS"
	LIBS="$LIBS `echo $HEPMCLIBS | sed -e 's! -R.* ! !'`"
	LDFLAGS="$LDFLAGS"
	CPPFLAGS="$CPPFLAGS $HEPMCINCLUDE"


if test "x$with_hepmcversion" = "x2"; then
	AC_CHECK_HEADERS([HepMC/HepMCDefs.h],[],[AC_MSG_WARN([

*********************************************************************
* HepMC versions before 2.05 may still work, but are not supported. *
*********************************************************************
])])
	# check HepMC
	AC_MSG_CHECKING([that HepMC works])
	AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <HepMC/GenEvent.h>
]],[[HepMC::GenEvent(HepMC::Units::GEV, HepMC::Units::MM);]])],[AC_MSG_RESULT([yes])],[
AC_MSG_RESULT([no]) 
	AC_MSG_ERROR([Use '--with-hepmc=' to set a path or use '--without-hepmc'.])
	])
    AC_CHECK_HEADERS([HepMC/PdfInfo.h],[],[AC_MSG_ERROR([Need HepMC with PdfInfo support.])],[#include <iostream>
])

fi

if test "x$with_hepmcversion" = "x3"; then

	# check HepMC
	AC_MSG_CHECKING([that HepMC3 works])
	AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <HepMC3/GenEvent.h>]],[[HepMC3::GenEvent(HepMC3::Units::GEV, HepMC3::Units::MM);]])],
	[
	AC_MSG_RESULT([yes])
	],
	[
    AC_MSG_RESULT([no]) 
	AC_MSG_ERROR([Use '--with-hepmc=' to set a path or use '--without-hepmc'.])
	])
       HEPMCROOTIO=0
       AC_CHECK_HEADERS([HepMC3/WriterRoot.h],    
       [ 
         HEPMCROOTIO=1
         AC_MSG_RESULT([HepMC3 has ROOT support.])
       ],[])
       AC_CHECK_HEADERS([HepMC3/WriterRootTree.h],
       [       
        HEPMCROOTIO=1
        AC_MSG_RESULT([HepMC has ROOT Tree support.])
       ],[])
       
       AC_CHECK_HEADERS([HepMC3/Writer.h],    
       [      
	 AC_DEFINE([HAVE_HEPMC3],  [1],[ We have HepMC3 ])
     AC_DEFINE([HEPMC_HAS_CROSS_SECTION],  [1],[ Has GenCrossection ])
     AC_DEFINE([HEPMC_HAS_NAMED_WEIGHTS],  [1],[ Has named weights ])
     AC_DEFINE([HEPMC_HAS_PDF_INFO],  [1],[ Has GenPdfInfo ])
     AC_DEFINE([HEPMC_HAS_UNITS] ,  [1],[ Has units ])
],[])
 
       
       
fi


	LIBS="$oldLIBS"
	LDFLAGS="$oldLDFLAGS"
	CPPFLAGS="$oldCPPFLAGS"
fi

AM_CONDITIONAL(HAVE_HEPMC,[test "x$with_hepmc" != "xno"])
AM_CONDITIONAL(HAVE_HEPMCROOTIO,[test "x$with_hepmc" != "xno"  && test "$HEPMCROOTIO" != "0"  ])
AC_SUBST(HEPMCINCLUDE)
AC_SUBST(HEPMCLIBS)
AC_SUBST(CREATE_HEPMC)
AC_SUBST(LOAD_HEPMC)
])
