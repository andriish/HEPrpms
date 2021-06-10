%define major       0
%define libname     Rivet
%define libnamedev  Rivet-devel
%define develnamestatic  Rivet-static-devel
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-python-bytecompile[[:space:]].*$!!g')


Name:           Rivet
Version:        3.1.4
Release:        1002%{?dist}
License:        GPLv3
Url:            http://rivet.hepforge.org/
Source0:        https://rivet.hepforge.org/downloads/%{name}-%{version}.tar.gz

Prefix: %{_prefix}
Summary:        Robust Independent Validation of Experiment and Theory
Requires:       fastjet    ImageMagick tex(pdflatex)
Requires:       YODA >= 1.8.0
Requires:       fjcontrib
BuildRequires:  fjcontrib fjcontrib-devel
BuildRequires:  binutils 
BuildRequires:  YODA >= 1.8.0 
BuildRequires:  YODA-devel >= 1.8.0
BuildRequires:  automake autoconf libtool  fastjet-devel  fastjet 
BuildRequires:  rsync
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0}
Requires:       zlib gsl
BuildRequires:  zlib zlib-devel gsl gsl-devel 
Requires:       HepMC3
Requires:       HepMC3-search
BuildRequires:  HepMC3-devel
BuildRequires:  HepMC3-search-devel
%endif
%if 0%{?suse_version}
Requires:       pkgconfig(zlib) pkgconfig(gsl)
BuildRequires:  pkgconfig(zlib) zlib-devel pkgconfig(gsl) gsl-devel 
Requires: libHepMC3-1 
BuildRequires: HepMC3-devel libHepMC3-1 
%endif

%if %{?rhel}%{!?rhel:0} >= 8
BuildRequires: python3 python3-devel  platform-python-devel python2-Cython
Requires:      python3-YODA
%endif
%if %{?fedora}%{!?fedora:0}
BuildRequires:   Cython python3 python3-devel 
Requires:        python3-YODA
%endif
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
BuildRequires: gcc-c++ gcc-gfortran
%endif
%if 0%{?suse_version}
BuildRequires: gcc-c++ gcc-fortran python3-Cython python3-devel
%endif


%description
The Rivet project (Robust Independent Validation of Experiment and Theory) 
is a toolkit for validation of Monte Carlo event generators. It provides a
large (and ever growing) set of experimental analyses useful for MC generator 
development, validation, and tuning, as well as a convenient infrastructure for
 adding your own analyses. Rivet is the most widespread way by which analysis code 
 from the LHC and other high-energy collider experiments is preserved for comparison 
 to and development of future theory models. 




%package  devel
Summary:        Libraries and headers for %{name}
Provides:       %{name}-devel = %{version}-%{release}
Requires:       YODA-devel >= 1.8.0
%description devel
%{libnamedev} contains the libraries and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.


%prep
%setup -q -n %{name}-%{version}
%build

sed -i 's|\$(prefix)\/etc|\$(sysconfdir)|g'   bin/Makefile.am

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
export PYTHON=%{_bindir}/python3
export CXXFLAGS="-g -Wformat -Wno-error -fPIC"  
pathfix.py -pn -i %{__python3}  ./
pathfix.py -pn -i %{__python3}  bin/rivet*
pathfix.py -pn -i %{__python3}  bin/make-*

autoreconf --force --install --verbose .
automake -a --force
%configure  --disable-doxygen --with-yoda=$(yoda-config --prefix ) --with-hepmc3=$(HepMC3-config --prefix) --with-fjcontrib=/usr --with-fastjet=$(fastjet-config --prefix)
make %{?_smp_mflags}
%endif


%if 0%{?suse_version}
export PYTHON_VERSION=%{py3_ver}
export PYTHON=%{_bindir}/python%{py3_ver}
export CXXFLAGS="-Wno-error -fPIC"  
autoreconf --force --install --verbose .
automake -a --force
%configure  --disable-doxygen --with-yoda=$(yoda-config --prefix ) --with-hepmc3=$(HepMC3-config --prefix) --with-fjcontrib=/usr --with-fastjet=$(fastjet-config --prefix)
make %{?_smp_mflags}
%endif


%install
%make_install 


%files 
%{_bindir}/*
%{_libdir}/*
%{_sysconfdir}/bash_completion.d/*
/usr/share/Rivet/*

%files -n %{libnamedev}
%{_includedir}/Rivet/*


%changelog
* Thu Jun 10 2021 Andrii Verbytskyi 3.1.4
- Added latex to dependencies
* Sat Mar 13 2021 Andrii Verbytskyi 3.1.3
- Added python3
* Wed Nov 27 2019 Andrii Verbytskyi 3.0.2
+ Initial spec file for 3.0.2
* Wed Oct 9 2019 Andrii Verbytskyi 3.0.1
+ Initial spec file for 3

