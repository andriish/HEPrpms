AC_INIT(APPLgrid, 1.6.27, sutt@cern.ch,applgrid,https://applgrid.hepforge.org)
AM_INIT_AUTOMAKE
AM_CONFIG_HEADER(amconfig.h:amconfig.in)
#AC_PROG_FC
#AC_CHECK_PROGS(FC, [gfortran g77 f77 fort77 f90], no)
#AC_PROG_FC
#AC_LANG_PUSH(Fortran)
#AC_FC_SRCEXT(f90)
#AC_FC_LIBRARY_LDFLAGS
#AC_FC_WRAPPERS
#AC_FC_FREEFORM
#AC_FC_DUMMY_MAIN
#AC_LANG_POP(Fortran)
AC_PROG_CC
AC_PROG_CXX 
AM_PROG_LIBTOOL
AM_SILENT_RULES([no])
AC_HEADER_STDC
AC_FUNC_UTIME_NULL
AC_CONFIG_MACRO_DIR([m4])
## Set Fortran compiler behaviour
#if test "x$FCFLAGS" == "x"; then 
#  FCFLAGS="-O2"
#fi
## Try to respect users' Fortran compiler variables
#if test "x$FC" == "x"; then 
#  if test "x$F77" == "x"; then 
#FC="$GFORTRAN"
#FC=gfortran
#  else
#    FC="$F77"
#  fi
#fi


CFLAGS=" -I/opt/local/include $CFLAGS " 
CXXFLAGS=" -I/opt/local/include $CXXFLAGS " 
LDFLAGS=" -L/opt/local/lib $LDFLAGS -lpthread "

# check for zlib
AC_SEARCH_LIBS( gzopen, z )

# AX_CHECK_ZLIB([ZLIBTEST=true], ZLIBTEST="")
# AX_CHECK_ZLIB()
# AC_MSG_NOTICE([ZLIBTEST is set to $ZLIBTEST])
# AM_CONDITIONAL(HAVE_LIBZ, test "x$ZLIBTEST" != "x")


AC_ARG_WITH([root],
            [AS_HELP_STRING([--without-root],[disable support for root])],
            [],
            [with_root=yes])
          
USEROOT=
AS_IF([test "x$with_root" != xno],
      [AC_SUBST([USEROOT], [1])],
      [AC_SUBST([USEROOT], [0])])

AC_MSG_CHECKING([compile with root]) 
AC_MSG_RESULT([$with_root]) 

			        
ROOT_CXXFLAGS=
ROOT_LDFLAGS=

if test "$USEROOT" == "1"; then 
AH_TEMPLATE([HAVE_CERNROOT],[Define 1 if the cern root package is installed])
AC_PATH_PROG(ROOTPATH,root,no)
if test "$ROOTPATH" = "no"; then
    AC_MSG_WARN([**************************])
    AC_MSG_WARN([])
    AC_MSG_WARN([root was not found ])
    AC_MSG_WARN([])
    AC_MSG_WARN([**************************])
    AC_SUBST([USEROOT], [0])
    echo "" > appl_grid/appl_root.h
else
    echo "#define USEROOT" > appl_grid/appl_root.h
    ROOT_CXXFLAGS=$(root-config --cflags)
    ROOT_LDFLAGS=$(root-config --ldflags)
    ROOT_LDFLAGS+=" " 
    ROOT_LDFLAGS+=$(root-config --libs)
    AC_DEFINE_UNQUOTED(HAVE_CERNROOT,1,[Define 1 if root is installed]) 
fi
else
    echo "" > appl_grid/appl_root.h
fi 

if test "$USEROOT" != "1"; then 
    echo "" > appl_grid/appl_root.h
    AH_TEMPLATE([HAVE_CERNROOT],[Define 0 if the cern root package is installed])
    AC_MSG_WARN([****************************************************************])
    AC_MSG_WARN([ compiling without root])
    if ( echo $CXXFLAGS | grep -q "std=c++" ); then 
       AC_MSG_WARN([ using $CXXFLAGS ])
    else
       touch /tmp/t.cxx 
       ( g++ -std=c++11 -c /tmp/t.cxx ) && COMP=" -std=c++11 "
       ( g++ -std=c++1y -c /tmp/t.cxx ) && COMP=" -std=c++1y "
       ( g++ -std=c++1z -c /tmp/t.cxx ) && COMP=" -std=c++1z "
       CXXFLAGS+=$COMP
       AC_MSG_WARN([ defaulting to $COMP : for a different c++ version ])
       AC_MSG_WARN([ should set CXXFLAGS+=" -std=<c++_version> " and run ])
       AC_MSG_WARN([ configure again] )
    fi
    AC_MSG_WARN([****************************************************************])
fi

( echo $CXXFLAGS | grep -q "m64" ) || CXXFLAGS+=" -m64 " 


AC_SUBST(ROOT_LDFLAGS)
AC_SUBST(ROOT_CXXFLAGS)


AM_CONDITIONAL(USE_ROOT, [test "$USEROOT" == "1"] )


# lhapdf isn't technically needed 
# AH_TEMPLATE([HAVE_LHAPDF],[Define 1 if lhapdf is installed])
# AC_PATH_PROG(LHAPDFPATH,lhapdf-config,no)

HOPPET_LDFLAGS=
HOPPET_CXXFLAGS=
AH_TEMPLATE([HAVE_HOPPET],[Define 1 if the hoppet library is installed])
AC_PATH_PROG(HOPPETPATH,hoppet-config,no)
if test "$HOPPETPATH" = "no"; then
    AC_MSG_WARN([********************************************************************])
    AC_MSG_WARN([])
    AC_MSG_WARN([   HOPPET was not found - you will not be able to arbirarily])
    AC_MSG_WARN([   change the factorisation scale in the convolution])
    AC_MSG_WARN([])
    AC_MSG_WARN([********************************************************************])
else 
     HOPPET_LDFLAGS=$(hoppet-config --libs)
     HOPPET_CXXFLAGS=$(hoppet-config --cxxflags) 
     AC_DEFINE_UNQUOTED(HAVE_HOPPET,1,[Define 1 if hoppet is installed]) 
fi

AC_SUBST(HOPPET_LDFLAGS)
AC_SUBST(HOPPET_CXXFLAGS)
AM_CONDITIONAL(USE_HOPPET, [test "$HOPPETPATH" != "no"] )


# check for the location of the libgfortran.{so,a} libraries
# would not be needed but some installations have only libgfortran.a
# and some only libgfortran.so, so we can'r simply use -print-file-name 
# to get the path.

AC_MSG_NOTICE([Checking for gfortran library ])

FRTLIB=$(gfortran --print-file-name=libgfortran.so ) 
FRTLPATH=$( echo $FRTLIB | sed 's|libgfortran.so||')

if test "$FRTLPATH" != ""; then
    AC_MSG_NOTICE([found libgfortran.so])
    AM_CONDITIONAL(USE_FRTLSO, [test "$FRTLPATH" != ""])
    AC_SUBST([FRTLLIB],[" -L$(\cd $FRTLPATH && \pwd) -lgfortran "])
else
    AC_MSG_WARN([libgfortran.so not found - checking for libgfotran.a])
    FRTLIB=$(gfortran --print-file-name=libgfortran.a)
    FRTLPATH=$(echo $FRTLIB | sed 's|libgfortran.a||')
    if test "$FRTLPATH" != ""; then
        AM_CONDITIONAL(USE_FRTLSO, [! test "$FRTLPATH" != ""])
        AC_MSG_NOTICE([found libgfortran.a])
        AC_SUBST([FRTLLIB],[" -L$(\cd $FRTLPATH && \pwd) -lgfortran "])
    else
        AM_CONDITIONAL(USE_FRTLSO, [test "x$FRTLPATH" != "x"])
        AC_SUBST([FRTLLIB],[""])
        AC_MSG_WARN([not found gfortran library - may not be able to link against fortran code or hoppet])
    fi
fi





LHAPDF_MAJOR_VERSION=0
LHAPDF_CXXFLAGS=
AH_TEMPLATE([HAVE_LHAPDF],[Define 1 if the LHAPDF library is installed])
AC_PATH_PROG(LHAPDFPATH,lhapdf-config,no)
if test "$LHAPDFPATH" != "no"; then

    # get LHAPDF major version - do nothing is version 6 not installed	
    LHAPDF_MAJOR_VERSION=$(lhapdf-config --version | sed 's/\..*//')

    if test $LHAPDF_MAJOR_VERSION -gt 5; then

       # find include path
       LHAPDF_CXXFLAGS=$(lhapdf-config --libs)

       AC_DEFINE_UNQUOTED(HAVE_LHAPDF,1,[Define 1 if LHAPDF is installed]) 
    
       LHAPDF_VERSION=$(lhapdf-config --version)

       AH_TEMPLATE([LHAPDF_VERSION],[LHAPDF version])
       AC_DEFINE_UNQUOTED(LHAPDF_VERSION,"$LHAPDF_VERSION",[LHAPDF version]) 

    fi
fi

AC_SUBST(LHAPDF_CXXFLAGS)
AM_CONDITIONAL(USE_LHAPDF, [test $LHAPDF_MAJOR_VERSION -gt 5 ] )


AC_CONFIG_FILES([bin/Makefile bin/applgrid-config])

AC_OUTPUT(Makefile src/Makefile)
