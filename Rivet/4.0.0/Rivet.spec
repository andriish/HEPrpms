%define libname     Rivet
%define libnamedev  Rivet-devel
%global debug_package %{nil}
%global __spec_install_post   /usr/lib/rpm/check-buildroot 

Name:           Rivet
Version:        4.0.0
Release:        1006%{?dist}
License:        GPLv3
Url:            http://rivet.hepforge.org/
Source0:        https://rivet.hepforge.org/downloads/%{name}-%{version}.tar.gz
Patch0:         patch-Rivet-0.txt

Prefix: %{_prefix}
Summary:        Robust Independent Validation of Experiment and Theory
Requires:       fastjet    ImageMagick tex(latex)  which
Requires:       YODA >= 2.0.0
Requires:       fjcontrib
BuildRequires:  fjcontrib fjcontrib-devel
BuildRequires:  binutils 
BuildRequires:  YODA >= 2.0.0 
BuildRequires:  YODA-devel >= 2.0.0
BuildRequires:  automake autoconf libtool  fastjet-devel  fastjet 
BuildRequires:  rsync
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0}
Requires:       zlib gsl hdf5 highfive-devel
BuildRequires:  zlib zlib-devel gsl gsl-devel hdf5-devel highfive-devel
Requires:       HepMC3
Requires:       HepMC3-search
BuildRequires:  HepMC3-devel
BuildRequires:  HepMC3-search-devel
%if %{?fedora}%{!?fedora:0} >= 39
BuildRequires: python3-rpm-macros
%endif
%endif
%if 0%{?suse_version}
Requires:       pkgconfig(zlib) pkgconfig(gsl)
BuildRequires:  pkgconfig(zlib) zlib-devel pkgconfig(gsl) gsl-devel  python3-setuptools
Requires: libHepMC3-3 
BuildRequires: HepMC3-devel libHepMC3-3 
%endif

%if %{?rhel}%{!?rhel:0} >= 8
BuildRequires: python3 python3-devel  platform-python-devel python3-Cython
Requires:      python3-YODA
%endif
%if %{?fedora}%{!?fedora:0}
%if %{?fedora}%{!?fedora:0} >= 35
BuildRequires: python-setuptools
%endif
BuildRequires:   Cython python3 python3-devel python3-libs 
Requires:        python3-YODA python3-matplotlib
%endif
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
BuildRequires: gcc-c++ gcc-gfortran
%endif
%if 0%{?suse_version}
BuildRequires: gcc-c++ gcc-fortran python3-Cython python3-devel  python3-setuptools
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
Requires:       YODA-devel >= 2.0.0
Requires:       %{name} = %{version}-%{release}
Requires:       highfive-devel hdf5-devel
%description devel
%{libnamedev} contains the libraries and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.


%prep
%setup -q -n %{name}-%{version}
%patch -P 0 -p1

%build

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
export PYTHON=%{_bindir}/python3
export CXXFLAGS="-g -Wformat -Wno-error -fPIC"  
%if %{?fedora}%{!?fedora:0} >= 39
%py3_shebang_fix  ./
%py3_shebang_fix  bin/rivet*
%py3_shebang_fix  bin/make-*
%else
pathfix.py -pn -i %{__python3}  ./
pathfix.py -pn -i %{__python3}  bin/rivet*
pathfix.py -pn -i %{__python3}  bin/make-*
%endif
%configure  --disable-doxygen --with-yoda=$(yoda-config --prefix ) --with-hepmc3=$(HepMC3-config --prefix) --with-fjcontrib=/usr --with-fastjet=$(fastjet-config --prefix)
make %{?_smp_mflags}
%endif


%if 0%{?suse_version}
export PYTHON_VERSION=%{py3_ver}
export PYTHON=%{_bindir}/python%{py3_ver}
export CXXFLAGS="-Wno-error -fPIC"  
%configure  --disable-doxygen --with-yoda=$(yoda-config --prefix ) --with-hepmc3=$(HepMC3-config --prefix) --with-fjcontrib=/usr --with-fastjet=$(fastjet-config --prefix)
make %{?_smp_mflags}
%endif


%install
%make_install 
find %{buildroot}/%{_libdir}/ -name "*.la" -delete
rm -fr %{buildroot}/%_libdir/python*/site-packages/__pycache__  
rm -fr %{buildroot}/%_libdir/python*/site-packages/easy-install.pth
rm -fr %{buildroot}/%_libdir/python*/site-packages/site.py

%files 
%{_bindir}/*
%_libdir/pkgconfig/*
%_libdir/*.*so*
%_libdir/python*/site-packages/*
%_libdir/Rivet/*
%{_sysconfdir}/bash_completion.d/*
/usr/share/Rivet/*

%files -n %{libnamedev}
%{_includedir}/Rivet/*


%changelog
* Wed Apr 26 2023 Andrii Verbytskyi 3.1.8
- Bump to 3.1.8
* Fri Sep 30 2022 Andrii Verbytskyi 3.1.7
- Bump to 3.1.7
* Wed Jul 27 2022 Andrii Verbytskyi 3.1.6
- Bump to 3.1.6
* Mon Nov 15 2021 Andrii Verbytskyi 3.1.5
- Bump to 3.1.5
* Thu Jun 10 2021 Andrii Verbytskyi 3.1.4
- Added latex to dependencies
* Sat Mar 13 2021 Andrii Verbytskyi 3.1.3
- Added python3
* Wed Nov 27 2019 Andrii Verbytskyi 3.0.2
+ Initial spec file for 3.0.2
* Wed Oct 9 2019 Andrii Verbytskyi 3.0.1
+ Initial spec file for 3

