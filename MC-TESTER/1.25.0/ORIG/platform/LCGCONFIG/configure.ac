# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT( [MC-TESTER], [1.24], [tomasz.przedzinski@uj.edu.pl] )
AC_CONFIG_AUX_DIR([config])

AM_INIT_AUTOMAKE

AC_LANG(C++)
#AC_LANG(Fortran)

AC_PREFIX_DEFAULT([`pwd`]) # setting $prefix here

AC_ARG_WITH([hepmc],
	    [ AS_HELP_STRING([--with-hepmc],
			     [path to HepMC 2.* installation]
			    )
	    ],
	    [],
	    [with_hepmc=no]
	   )

#AM_CONDITIONAL([HEPMC_INTERFACE], [test "x$with_hepmc" != "xno"])

AS_IF([test "x$with_hepmc" != xno], [AC_SUBST(HEPMC_DIR,[$with_hepmc])],
      [echo "WARNING: path to HepMC installation is not given. Won't compile HepMC interface">&2 ; ]
     )

if test "x$with_hepmc" != "xno" ; then
	echo "Checking for HepMC headers ... "
	if test -s ${HEPMC_DIR}/include/HepMC/Version.h ; then
		echo found
	else
		echo NOT found
		echo ERROR: cannot find HepMC include directory >&2
		exit 1
	fi
	echo "Checking for a presence of HepMC library in ${HEPMC_DIR}/lib/ ...  "
	if ls ${HEPMC_DIR}/lib/libHepMC.* >&/dev/null ; then
		echo libHepMC found
	else
		echo libHepMC NOT found
		echo ERROR: cannot find "libHepMC.*" libraries >&2
		exit 1
	fi
fi
AM_CONDITIONAL([HAS_HEPMC],[test "x$with_hepmc" != "xno"])
			     
AC_ARG_WITH([pythia8],
	    [ AS_HELP_STRING([--with-pythia8],
			     [path to Pythia8 installation (optional, nedeed to compile examples)]
			    )
	    ],
	    [],
	    [with_pythia8=no]
	   )


AC_ARG_WITH([root],
	    [ AS_HELP_STRING([--with-root],
			     [path to ROOT installation]
			    )
	    ],
	    [],
	    [with_root=no]
	   )

AS_IF([test "x$with_root" != xno], [AC_SUBST(ROOTSYS,[$with_root])],
      [echo "ERROR: path to ROOT installation is not given. Can't proceed w/o ROOT.">&2; exit 1; ]
     )

AM_CONDITIONAL([HAS_ROOT],[test "x$with_root" != "xno"])

if test "x$with_root" != "xno" ; then
	echo "Checking for root-config ... "

	if test -x ${ROOTSYS}/bin/root-config ; then 
		echo found
	else
		echo NOT found
		echo ERROR: cannot find root-config in ${ROOTSYS}/bin/ >&2
		exit 1
	fi

	echo "Checking for ROOT bin/ directory ... "
	_root_bin_=`${ROOTSYS}/bin/root-config --bindir`
	if test "x$_root_bin_" = "x" ; then
		echo "ERROR: root-config --bindir returns nothing. Check your ROOT installation." >&2
		exit 1
	fi
	echo ${_root_bin_}
	AC_SUBST(ROOT_BIN,[$_root_bin_])

	echo "Checking for ROOT lib/ directory ... "
	_root_lib_=`${ROOTSYS}/bin/root-config --libdir`
	if test "x$_root_lib_" = "x" ; then
		echo "ERROR: root-config --libdir returns nothing. Check your ROOT installation." >&2
		exit 1
	fi
	echo ${_root_lib_}
	AC_SUBST(ROOT_LIBDIR,[$_root_lib_])

	echo "Checking for rootcint ... "
	if test -x ${_root_bin_}/rootcint ; then
		echo found.
	else
		echo " NOT found in ${_root_bin_}. Check your ROOT installation">&2
		exit 1
	fi
	AC_SUBST(ROOT_CINT,[${_root_bin_}/rootcint])

	echo "Checking for ROOT lib's ..."

	_root_libs_=`${ROOTSYS}/bin/root-config --libs`
	if test "x$_root_libs_" = "x" ; then
		echo "WARNING: root-config --libs returns nothing. Let's trust it for a while. " >&2
	else
		echo "$_root_libs_"
	fi
	AC_SUBST(ROOT_LIBS,[$_root_libs_])

	echo "Checking for ROOT compiler flags ... "

	_root_cflags_=`${ROOTSYS}/bin/root-config --cflags`
	if test "x$_root_cflags_" = "x" ; then
		echo "WARNING: root-config --cflags returns nothing. Let's trust it for a while. " >&2
	else
		echo "$_root_cflags_"
	fi
	AC_SUBST(ROOT_CFLAGS,[$_root_cflags_])
fi

#AM_CONDITIONAL([HEPMC_INTERFACE], [test "x$with_hepmc" != "xno"])

AS_IF([test "x$with_pythia8" != xno], [AC_SUBST(PYTHIA8_DIR,[$with_pythia8])],
      [echo "WARNING: path to Pythia 8 installation is not given. Won't be able to compile examples.">&2 ; ]
     )
			     
#AM_CONDITIONAL([HEPMC_INTERFACE], [test "x$with_hepmc" != "xno"])

AS_IF([test "x$with_pythia8" != xno], [AC_SUBST(PYTHIA8_DIR,[$with_pythia8])],
      [echo "WARNING: path to Pythia 8 installation is not given. Won't be able to compile examples.">&2 ; ]
     )
			     
AC_CONFIG_SRCDIR([src/Setup.cxx])
AC_CONFIG_HEADERS([config/config.h])

AC_PROG_CXX

if ${CXX} --version | grep -E "GCC.* 3" >&/dev/null ; then
	AC_PROG_F77([g77 gfortran f77 fortran fort])
else
	AC_PROG_F77([gfortran g77 f77 fortran fort])
fi

AC_PROG_LIBTOOL

AC_PROG_INSTALL
AC_PROG_LN_S

AM_CONDITIONAL([DO_INSTALL],[test "x$prefix" != "xNONE"])

AC_CONFIG_FILES([Makefile src/Makefile HepMCEvent/Makefile HEPEvent/Makefile include/Makefile])


AC_OUTPUT

rm -f examples-*/paths.sh
(
 echo "MCTESTER_DIR=$prefix" ;
 echo "HEPMC_DIR=$HEPMC_DIR" ;
 if test "x$with_root" != "xno" ; then
	 echo "ROOTSYS=\"$ROOTSYS\" ;"
	 echo "ROOT_BIN=\"$ROOT_BIN\" ;"
	 echo "ROOT_CFLAGS=\"$ROOT_CFLAGS\" ;"
	 echo "ROOT_CINT=\"$ROOT_CINT\" ;"
	 echo "ROOT_LIBDIR=\"$ROOT_LIBDIR\" ;"
	 echo "ROOT_LIBS=\"$ROOT_LIBS\" ;"
 fi
 if test "x$with_pythia8" != "xno" ; then
	echo "PYTHIA8_DIR=\"$with_pythia8\"" ;
 fi
) > examples-F77/paths.sh
cp  examples-F77/paths.sh examples-C++/paths.sh
