# Please note that this .spec file was designed to work in the CI  with 
# specific versions of RHEL/Fedora and, probably,  will need some 
# adjustements to work properly with all distributions

%if %{?rhel}%{!?rhel:0} == 4 || %{?rhel}%{!?rhel:0} == 5 || %{?rhel}%{!?rhel:0} == 6 
%global cmake   /usr/bin/cmake 
%global cmake_install make install DESTDIR=${RPM_BUILD_ROOT}
%global __cmake cmake
%global __cmake_builddir .
%global cmakeB %__cmake  --build %{__cmake_builddir} -- %{?_smp_mflags} 
%endif

%if %{?fedora}%{!?fedora:0} == 30
%global __cmake_builddir .
%global cmakeB %__cmake  --build %{__cmake_builddir}  %{?_smp_mflags} 
%global cmake_install make install DESTDIR=${RPM_BUILD_ROOT}
%endif

%if %{?rhel}%{!?rhel:0} > 6 || %{?fedora}%{!?fedora:0} > 30
%global cmakeB %__cmake  --build %{__cmake_builddir}  %{?_smp_mflags} 
%endif

# compiler is used to disambiguate package names and executables
%if %{?rhel}%{!?rhel:0} > 4 || %{?fedora}%{!?fedora:0} > 6 || 0%{?suse_version}
%define compiler_string -gfortran
%define compiler -gfortran
%else
%define compiler_string -g77
%define compiler -g77
%endif

%if %{?rhel}%{!?rhel:0} < 5
%define no_build_id 1
%endif

#define verdir {version}
%define verdir 2023
%define verdatadir 2023.08

# data files should be the same and therefore parallel installable

Name:          cernlib
Version:       2023.08.14.0
Release:       1%{?dist}
Summary:       General purpose CERN library
Group:         Development/Libraries
License:       GPL+ and LGPLv2+
URL:           http://cernlib.web.cern.ch/cernlib/
Patch0:         patch-cernlib-0.txt

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 7
BuildRequires: openssl-libs openssl-devel  lapack-devel blas-devel lapack blas
%endif

%if 0%{?suse_version}
BuildRequires: lapacke-devel
BuildRequires: cmake
BuildRequires: openssl libopenssl-devel lapack-devel blas-devel
%endif

%if %{?rhel}%{!?rhel:0} == 4 
BuildRequires: xorg-x11-devel 
%else
BuildRequires: libXaw-devel
BuildRequires: libXau-devel
%endif

# indirectly requires lesstif or openmotif and X libs
BuildRequires: xbae-devel

# for patchy build scripts
BuildRequires: gawk

BuildRequires: desktop-file-utils
%if %{?fedora}%{!?fedora:0} >=1 
BuildRequires: cmake
BuildRequires: chrpath
%endif
%if %{?rhel}%{!?rhel:0} >= 8
BuildRequires: cmake
%endif
%if %{?rhel}%{!?rhel:0} == 7
BuildRequires: cmake3
%endif




%if %{?fedora}%{!?fedora:0} >=28 || %{?rhel}%{!?rhel:0} >= 8
BuildRequires: libnsl libnsl2-devel make
%endif


%if %{?rhel}%{!?rhel:0} >4 || 0%{?fedora}
BuildRequires: gcc-gfortran
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-fortran
%endif
%if %{?rhel}%{!?rhel:0} == 4 
BuildRequires: /usr/bin/g77
%endif

BuildRoot:     %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)

Source0:    https://cernlib.web.cern.ch/cernlib/download/2023_source/tar/cernlib-%{version}-free.tar.gz


%description
CERN program library is a large collection of general purpose libraries
and modules maintained and offered on the CERN. Most of these programs 
were developed at CERN and are therefore oriented towards the needs of a 
physics research laboratory that is general mathematics, data analysis, 
detectors simulation, data-handling etc... applicable to a wide range 
of problems.

The main and devel packages are parallel installable, but not the helper
scripts from the utils subpackage.

%package devel
Summary:       General purpose CERN library development package

%if %{?rhel}%{!?rhel:0} == 4 
Requires:       lapack blas
%else
Requires:       lapack-devel blas-devel
%endif

%if 0%{?fedora} >= 28 || %{?rhel}%{!?rhel:0} >= 8
Requires: libnsl2-devel
%endif
%if 0%{?suse_version}
BuildRequires: libnsl-devel
%endif
# Motif and X devel libs are indirectly required through xbae
Requires: xbae-devel

%if %{?rhel}%{!?rhel:0} == 4 
Requires:      xorg-x11-devel
%else
# workaround #173530
Requires:      libXau-devel
Requires:      libXaw-devel
%endif
Requires:      %{name} = %{version}-%{release}
Group:         Development/Libraries
Provides:  cernlib(devel) = %{version}-%{release}

# for the m4 macro directory ownership
Requires: automake

%description devel
CERN program library is a large collection of general purpose libraries
and modules maintained and offered on the CERN. Most of these programs 
were developed at CERN and are therefore oriented towards the needs of a 
physics research laboratory that is general mathematics, data analysis, 
detectors simulation, data-handling etc... applicable to a wide range 
of problems.

The cernlib-devel package contains the header files and symlinks needed  
to develop programs that use the CERN library using dynamic libraries.

Static libraries are in %{name}-static.

%package static
Summary:       General purpose CERN library static libraries
Group:         Development/Libraries
Requires:  %{name}-devel = %{version}-%{release}
Provides:  cernlib(static) = %{version}-%{release}

%description static
CERN program library is a large collection of general purpose libraries
and modules maintained and offered on the CERN. Most of these programs 
were developed at CERN and are therefore oriented towards the needs of a 
physics research laboratory that is general mathematics, data analysis, 
detectors simulation, data-handling etc... applicable to a wide range 
of problems.

The %{name}-static package contains the static cernlib libraries.

%package utils
Summary:   CERN library compilation and environment setting scripts
Group:     Applications/System
Requires:  %{name}-devel = %{version}-%{release}
Requires:  %{name}-static = %{version}-%{release}
Provides:  cernlib(utils) = %{version}-%{release}

%description utils
CERN library compilation and environment setting scripts.

This package will conflict with other versions, therefore if you 
want to have different compile script and different environments for 
different versions of the library you have to set them by hand.

%package -n geant321%{?compiler}
Summary:  Particle detector description and simulation tool
Group:    Applications/Engineering
Requires: %{name}-devel = %{version}-%{release}
Requires: %{name}-utils = %{version}-%{release}

%description -n geant321%{?compiler}
Geant simulates the passage of subatomic particles through matter, for 
instance, particle detectors. For maximum flexibility, Geant simulations 
are performed by linking Fortran code supplied by the user with the Geant 
libraries, then running the resulting executable.

This package includes gxint, the script used to perform this linking step. 

%package -n kuipc%{?compiler}
Summary:  Cernlib's Kit for a User Interface Package (KUIP) compiler
Group:    Development/Languages
Requires: cernlib(devel)

%description -n kuipc%{?compiler}
KUIPC, the Kit for a User Interface Package Compiler, is a tool to simplify 
the writing of a program's user interface code. It takes as input a Command 
Definition File (CDF) that describes the commands to be understood by the 
program, and outputs C or FORTRAN code that makes the appropriate function 
calls to set up the user interface. This code can then be compiled and linked 
with the rest of the program. Since the generated code uses KUIP routines, 
the program must also be linked against the Packlib library that contains them.

# we want to have both g77 and gfortran suffixed utilities available.
%package -n paw%{?compiler_string}
Group: Applications/Engineering
Summary: A program for the analysis and presentation of data

%description -n paw%{?compiler_string}
PAW is conceived as an instrument to assist physicists in the analysis and 
presentation of their data. It provides interactive graphical presentation 
and statistical or mathematical analysis, working on objects familiar to 
physicists like histograms, event files (Ntuples), vectors, etc. PAW is 
based on several components of the CERN Program Library.

%package -n cernlib-packlib%{?compiler_string}
Group: Applications/Archiving
Summary: I/O, network and other utilities from the cernlib

%description -n cernlib-packlib%{?compiler_string}
I/O, network and miscalleneous utilities based on the CERN Program 
Library. 
According to the responsible of the cernlib debian package, some
of these utilities may have security flaws.

%package -n patchy%{?compiler_string}
Group: Applications/Archiving
Summary: The patchy utilities

%description -n patchy%{?compiler_string}
Utilities for extracting sources from patchy cards and cradles.

# the package that has utils_compiler set provides the utilities without
# suffix
%package -n paw
Group: Applications/Engineering
Summary: A program for the analysis and presentation of data

%description -n paw
PAW is conceived as an instrument to assist physicists in the analysis and 
presentation of their data. It provides interactive graphical presentation 
and statistical or mathematical analysis, working on objects familiar to 
physicists like histograms, event files (Ntuples), vectors, etc. PAW is 
based on several components of the CERN Program Library.

%package -n cernlib-packlib
Group: Applications/Archiving
Summary: I/O, network and other utilities from the cernlib

%description -n cernlib-packlib
I/O, network and miscalleneous utilities based on the CERN Program 
Library. 
According to the responsible of the cernlib debian package, some
of these utilities may have security flaws.

%package -n patchy
Group: Applications/Archiving
Summary: The patchy utilities

%description -n patchy
Utilities for extracting sources from patchy cards and cradles.



%prep
%setup -q -c 
%patch0 -p1


%build
find cernlib-%{version}-free -type f -name '.*' -exec rm -rf  {} \;
%if %{?rhel}%{!?rhel:0} == 4 || %{?rhel}%{!?rhel:0} == 5 || %{?rhel}%{!?rhel:0} == 6
find -name CMakeLists.txt -exec sed -i -e 's@list(TRANSFORM@list_TRANSFORM(@g' -e 's@list(JOIN@list_JOIN(@g' {} \; 
%endif
%if %{?rhel}%{!?rhel:0} == 4 || %{?rhel}%{!?rhel:0} == 5 || %{?rhel}%{!?rhel:0} == 6
%if %{?rhel}%{!?rhel:0} == 4
export FC=g77
%else
export FC=gfortran
%endif
cmake  -DCMAKE_VERBOSE_MAKEFILE:BOOL=OFF -S cernlib-%{version}-free -DCERNLIB_BUILD_SHARED=ON -DCERNLIB_USE_INTERNAL_XBAE=OFF -DCERNLIB_USE_INTERNAL_LAPACK=OFF  -DCMAKE_Fortran_COMPILER=${FC} -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_INSTALL_LIBDIR=%{_libdir}/cernlib/%{verdir}/lib -DCMAKE_INSTALL_INCLUDEDIR=%{_includedir}/cernlib/%{verdir} -DCMAKE_VERBOSE_MAKEFILE:BOOL=OFF
%cmakeB
%endif
%if 0%{?fedora} || %{?rhel}%{!?rhel:0} >= 7
export FC=gfortran
%cmake -DCMAKE_VERBOSE_MAKEFILE:BOOL=OFF -S cernlib-%{version}-free -DCERNLIB_BUILD_SHARED=ON -DCERNLIB_USE_INTERNAL_XBAE=OFF -DCERNLIB_USE_INTERNAL_LAPACK=OFF                                                         -DCMAKE_INSTALL_LIBDIR=%{_libdir}/cernlib/%{verdir}/lib -DCMAKE_INSTALL_INCLUDEDIR=%{_includedir}/cernlib/%{verdir} -DCMAKE_VERBOSE_MAKEFILE:BOOL=OFF
%cmakeB
%endif

%if 0%{?suse_version}
export FC=gfortran
%cmake -DCMAKE_VERBOSE_MAKEFILE:BOOL=OFF -S ../cernlib-%{version}-free -DCERNLIB_BUILD_SHARED=ON -DCERNLIB_USE_INTERNAL_XBAE=OFF -DCERNLIB_USE_INTERNAL_LAPACK=OFF                                                         -DCMAKE_INSTALL_LIBDIR=%{_libdir}/cernlib/%{verdir}/lib -DCMAKE_INSTALL_INCLUDEDIR=%{_includedir}/cernlib/%{verdir} -DCMAKE_VERBOSE_MAKEFILE:BOOL=OFF
%cmake_build
%endif

%install
%cmake_install
%{__install} -d -m755 %{buildroot}/etc/ld.so.conf.d
echo %{_libdir}/cernlib/%{version}/lib > %{buildroot}/etc/ld.so.conf.d/cernlib-%{version}-%{_arch}.conf
%if 0%{?fedora}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/kuipc
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/cdbackup%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/cdmake%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/cdmove%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/cdserv%{?compiler_string}
#chrpath --delete $RPM_BUILD_ROOT%{_bindir}/dzedit%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/dzeX11%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/fatback%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/fatmen%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/fatnew%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/fatsend%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/fatsrv%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/fcasplit%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/hepdb%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/kuesvr%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/kxterm%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/nycheck%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/nydiff%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/nyindex%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/nylist%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/nymerge%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/nypatchy%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/nyshell%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/nysynopt%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/nytidy%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/paw++.dynamic%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/paw++%{?compiler_string}
#chrpath --delete $RPM_BUILD_ROOT%{_bindir}/paw%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/pawserv%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/pawX11.dynamic%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/pawX11%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/ycompar%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/yedit%{?compiler_string}
#chrpath --delete $RPM_BUILD_ROOT%{_bindir}/yexpand%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/yfrceta%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/yindexb%{?compiler_string}
#chrpath --delete $RPM_BUILD_ROOT%{_bindir}/yindex%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/ylistb%{?compiler_string}
#chrpath --delete $RPM_BUILD_ROOT%{_bindir}/ylist%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/ypatchy%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/ysearch%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/yshift%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/ytobcd%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/ytobin%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/ytoceta%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/zftp%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_bindir}/zserv%{?compiler_string}
chrpath --delete $RPM_BUILD_ROOT%{_libdir}/cernlib/%{verdir}/lib/lib*so.*20*
%endif
%check
%if 0%{?fedora} || %{?rhel}%{!?rhel:0} >= 7
ctest  --output-on-failure --force-new-ctest-process -E 'testcojetst|testzebfz2|testkernnumtest|testgent|testerexam1|testerexam2'
%endif

%clean
rm -rf %{buildroot}

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%post -n paw%{?compiler_string}
touch --no-create %{_datadir}/icons/hicolor || :
%{_bindir}/gtk-update-icon-cache --quiet %{_datadir}/icons/hicolor || :

%postun -n paw%{?compiler_string}
touch --no-create %{_datadir}/icons/hicolor || :
%{_bindir}/gtk-update-icon-cache --quiet %{_datadir}/icons/hicolor || :

%if 0%{?utils_compiler}
%post -n paw
touch --no-create %{_datadir}/icons/hicolor || :
%{_bindir}/gtk-update-icon-cache --quiet %{_datadir}/icons/hicolor || :

%postun -n paw
touch --no-create %{_datadir}/icons/hicolor || :
%{_bindir}/gtk-update-icon-cache --quiet %{_datadir}/icons/hicolor || :
%endif

%files
%defattr(-,root,root,-)
%{_datadir}/doc/cernlib/cernlib.copyright
%{_datadir}/doc/cernlib/geant321.README
%{_datadir}/doc/cernlib/vim/*
/etc/ld.so.conf.d/*
%dir %{_libdir}/cernlib/
%dir %{_libdir}/cernlib/%{verdir}
%dir %{_libdir}/cernlib/%{verdir}/lib
%dir %{_libdir}/cernlib/%{verdir}/bin
%{_libdir}/cernlib/%{verdir}/lib/*.so.*
%{_datadir}/cernlib/%{verdatadir}/*dat
%dir %{_datadir}/cernlib
%dir %{_datadir}/cernlib/cmake
%{_datadir}/cernlib/cmake/CERNLIBConfig.cmake
%{_datadir}/cernlib/cmake/CERNLIBConfig-version.cmake

# the utils and devel are separated to have the possibility to install
# parallel versions of the library
%files devel
%defattr(-,root,root,-)
%{_datadir}/doc/cernlib-devel/cfortran/Examples/*
%{_datadir}/doc/cernlib-devel/comis-64bit-example.F
%{_libdir}/cernlib/%{verdir}/lib/*.so
%{_includedir}/cernlib/
%{_datadir}/aclocal/cernlib.m4

%files static
%defattr(-,root,root,-)
%{_libdir}/cernlib/%{verdir}/lib/*.a

%files utils
%defattr(-,root,root,-)
%{_datadir}/doc/cernlib-utils/cernlib-%{verdatadir}.csh 
%{_datadir}/doc/cernlib-utils/cernlib-%{verdatadir}.sh
%{_bindir}/cernlib*
%{_mandir}/man1/cernlib*.1*


%files -n geant321%{?compiler}
%defattr(-,root,root,-)
%{_bindir}/gxint
#{?compiler}
%{_datadir}/X11/app-defaults/*Geant++
%{_mandir}/man1/gxint.1*

%files -n kuipc%{?compiler}
%defattr(-,root,root,-)
%{_bindir}/kuipc
#{?compiler}
%{_mandir}/man1/kuipc.1*

%files -n paw%{?compiler_string}
%defattr(-,root,root,-)
%{_bindir}/paw++%{?compiler_string}
%{_bindir}/paw%{?compiler_string}
%{_bindir}/pawX11%{?compiler_string}
%{_bindir}/pawX11.dynamic%{?compiler_string}
%{_bindir}/paw++.dynamic%{?compiler_string}
# paw doesn't explicitly depend on the main package, so it owns the dirs
%dir %{_libdir}/cernlib/%{verdir}
%dir %{_libdir}/cernlib/%{verdir}/bin
%{_libdir}/cernlib/%{verdir}/bin/pawX11%{?compiler_string}
%{_datadir}/X11/app-defaults/*Paw++
%{_datadir}/icons/hicolor/
%{_mandir}/man1/paw*.1*
#{_datadir}/applications/*paw++{?compiler_string}.desktop
%{_datadir}/pixmaps/paw*.xpm

%files -n cernlib-packlib%{?compiler_string}
%defattr(-,root,root,-)
%{_datadir}/doc/cernlib-packlib%{?compiler_string}/zftp.README
%{_bindir}/cdbackup%{?compiler_string}
%{_bindir}/cdserv%{?compiler_string}
%{_bindir}/dzedit%{?compiler_string}
# packlib doesn't explicitly depend on the main package, so it owns the dirs
%dir %{_libdir}/cernlib/%{verdir}
%dir %{_libdir}/cernlib/%{verdir}/bin
%{_libdir}/cernlib/%{verdir}/bin/dzeX11%{?compiler_string}
%{_bindir}/dzeX11%{?compiler_string}
%{_bindir}/fatmen%{?compiler_string}
%{_bindir}/fatsrv%{?compiler_string}
%{_bindir}/kuesvr%{?compiler_string}
%{_bindir}/zserv%{?compiler_string}
%{_bindir}/cdmake%{?compiler_string}
%{_bindir}/fatnew%{?compiler_string}
%{_bindir}/pawserv%{?compiler_string}
%{_bindir}/cdmove%{?compiler_string}
%{_bindir}/fatback%{?compiler_string}
%{_bindir}/fatsend%{?compiler_string}
%{_bindir}/hepdb%{?compiler_string}
%{_bindir}/kxterm%{?compiler_string}
%{_bindir}/zftp%{?compiler_string}
%{_datadir}/X11/app-defaults/KXterm
%{_datadir}/pixmaps/kxterm*.xpm
%{_mandir}/man1/kxterm.1* 
%{_mandir}/man1/dze*.1*
%{_mandir}/man1/kuesvr.1* 
%{_mandir}/man1/zftp.1*
%{_mandir}/man8/*.8*

%files -n patchy%{?compiler_string}
%defattr(-,root,root,-)
%{_bindir}/fcasplit%{?compiler_string}
%{_bindir}/nycheck%{?compiler_string}
%{_bindir}/nydiff%{?compiler_string}
%{_bindir}/nyindex%{?compiler_string}
%{_bindir}/nylist%{?compiler_string}
%{_bindir}/nymerge%{?compiler_string}
%{_bindir}/nypatchy%{?compiler_string}
%{_bindir}/nyshell%{?compiler_string}
%{_bindir}/nysynopt%{?compiler_string}
%{_bindir}/nytidy%{?compiler_string}
%{_bindir}/yexpand*
#{_bindir}/yexpand{?compiler_string}
%{_bindir}/ycompar%{?compiler_string}
%{_bindir}/yedit%{?compiler_string}
%{_bindir}/yfrceta%{?compiler_string}
#{_bindir}/yindex{?compiler_string}
%{_bindir}/yindexb%{?compiler_string}
#{_bindir}/ylist{?compiler_string}
%{_bindir}/ylistb%{?compiler_string}
%{_bindir}/ypatchy%{?compiler_string}
%{_bindir}/ysearch%{?compiler_string}
%{_bindir}/yshift%{?compiler_string}
%{_bindir}/ytobcd%{?compiler_string}
%{_bindir}/ytobin%{?compiler_string}
%{_bindir}/ytoceta%{?compiler_string}
%{_mandir}/man1/ny*.1*
%{_mandir}/man1/yexpand.1* 
%{_mandir}/man1/ypatchy.1*

%if 0%{?utils_compiler}
%files -n paw
%defattr(-,root,root,-)
%{_bindir}/paw++
%{_bindir}/paw
%{_bindir}/pawX11
%{_bindir}/pawX11.dynamic
%{_bindir}/paw++.dynamic
# paw doesn't explicitly depend on the main package, so it owns the dirs
%dir %{_libdir}/cernlib/%{verdir}
%dir %{_libdir}/cernlib/%{verdir}/bin
%{_libdir}/cernlib/%{verdir}/bin/pawX11
%{_datadir}/X11/app-defaults/*Paw++
%{_datadir}/icons/hicolor/
%{_mandir}/man1/paw*.1*
#{_datadir}/applications/*paw++.desktop
%{_datadir}/pixmaps/paw*.xpm

%files -n cernlib-packlib
%defattr(-,root,root,-)
#{_datadir}/doc/cernlib/zftp.README
%{_bindir}/cdbackup
%{_bindir}/cdserv
%{_bindir}/dzedit
# packlib doesn't explicitly depend on the main package, so it owns the dirs
%dir %{_libdir}/cernlib/%{verdir}
%dir %{_libdir}/cernlib/%{verdir}/bin
%{_libdir}/cernlib/%{verdir}/bin/dzeX11
%{_bindir}/dzeX11
%{_bindir}/fatmen
%{_bindir}/fatsrv
%{_bindir}/kuesvr
%{_bindir}/zserv
%{_bindir}/cdmake
%{_bindir}/fatnew
%{_bindir}/pawserv
%{_bindir}/cdmove
%{_bindir}/fatback
%{_bindir}/fatsend
%{_bindir}/hepdb
%{_bindir}/kxterm
%{_bindir}/zftp
%{_datadir}/X11/app-defaults/KXterm
%{_datadir}/pixmaps/kxterm*.xpm
%{_mandir}/man1/kxterm.1* 
%{_mandir}/man1/dze*.1*
%{_mandir}/man1/kuesvr.1* 
%{_mandir}/man1/zftp.1*
%{_mandir}/man8/*.8*

%files -n patchy
%defattr(-,root,root,-)
%{_bindir}/fcasplit
%{_bindir}/nycheck
%{_bindir}/nydiff
%{_bindir}/nyindex
%{_bindir}/nylist
%{_bindir}/nymerge
%{_bindir}/nypatchy
%{_bindir}/nyshell
%{_bindir}/nysynopt
%{_bindir}/nytidy
%{_bindir}/yexpand
%{_bindir}/ycompar
%{_bindir}/yedit
%{_bindir}/yfrceta
#{_bindir}/yindex
%{_bindir}/yindexb
#{_bindir}/ylist
%{_bindir}/ylistb
%{_bindir}/ypatchy
%{_bindir}/ysearch
%{_bindir}/yshift
%{_bindir}/ytobcd
%{_bindir}/ytobin
%{_bindir}/ytoceta
%{_mandir}/man1/ny*.1*
%{_mandir}/man1/yexpand.1* 
%{_mandir}/man1/ypatchy.1*
%endif

%changelog
* Wed Sep 27 2023 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 2023-08
 - bump version
* Wed Oct 19 2022 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 2022-10
 - bump version
* Mon Jul 18 2022 Ulrich Schwickerath <ulrich.schwickerath@cern.ch> 2022-7
 - bump version
* Tue Dec 1 2020 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 2006-43
 - Adding CentOS7/CentOS8 support
