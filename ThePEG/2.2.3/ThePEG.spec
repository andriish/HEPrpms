%define libname     ThePEG
%define libnamedev  ThePEG-devel


Name:           ThePEG
Version:        2.2.3
Release:        1002%{?dist}
License:        GPLv3
Url:            http://www.hepforge.org/archive/thepeg
Source0:        https://thepeg.hepforge.org/downloads/%{name}-%{version}.tar.bz2
Patch1:         patch-ThePEG-1.txt
Summary:        Monte Carlo event generator toolkit



%if 0%{?rhel} || 0%{?fedora}
Requires:       lhapdf fastjet gsl zlib gsl fastjet libquadmath
BuildRequires:  autoconf automake    libtool  libquadmath-devel  libquadmath
BuildRequires:  gsl gsl-devel fastjet fastjet-devel zlib-devel lhapdf-devel  boost boost-devel
Requires:       MG5_aMC fjcontrib YODA
BuildRequires:  MG5_aMC fjcontrib YODA YODA-devel
Requires:       HepMC3  HepMC3-rootIO HepMC3-search   HepMC3-devel HepMC3-search-devel HepMC3-rootIO-devel 
BuildRequires:  HepMC3  HepMC3-rootIO HepMC3-search   HepMC3-devel HepMC3-search-devel HepMC3-rootIO-devel 
%endif
%if 0%{?suse_version}
Requires:       libLHAPDF fastjet gsl zlib gsl fastjet libquadmath0
BuildRequires:  autoconf automake    libtool    libquadmath0
BuildRequires:  gsl gsl-devel fastjet fastjet-devel zlib-devel LHAPDF-devel  libboost_test-devel
Requires:       MG5_aMC fjcontrib YODA
BuildRequires:  MG5_aMC fjcontrib YODA YODA-devel
Requires:       libHepMC3-3     HepMC3-devel  
BuildRequires:  libHepMC3-3     HepMC3-devel 
%endif
Requires:       Rivet >= 3.1.0  
Requires:       Rivet-devel >= 3.1.0  
BuildRequires:  Rivet >= 3.1.0  
BuildRequires:  Rivet-devel  >= 3.1.0

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
BuildRequires: gcc-c++ gcc-gfortran
%endif
%if 0%{?suse_version}
BuildRequires: gcc-c++ gcc-fortran
%endif
#Actually only Rivet+HepMC3 will work with ThePEG+HepMC3
Prefix: %{_prefix}

%description
 ThePEG is a toolkit for providing a common platform for using and 
 building event generators in C++. 


%package  devel
Summary:        Libraries and headers for %{name}
Provides:       %{name}-devel = %{version}-%{release}
Requires:       gsl-devel fastjet-devel

%description devel
%{libnamedev} contains the libraries and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.

#Yes, suse is stupid
%if 0%{?suse_version}

%debug_package

%endif


%prep
%setup -q
%patch1 -p1

%build



%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
touch configure.ac
autoreconf --force --install --verbose .
automake -a --force
%configure  --disable-rpath  --with-hepmc=%_prefix --with-rivet=%_prefix --with-fastjet=%_prefix  --with-lhapdf=/usr   --with-hepmcversion=3
make %{?_smp_mflags}
%endif
%if 0%{?suse_version}
touch configure.ac
autoreconf --force --install --verbose .
automake -a --force
%configure  --disable-rpath  --with-hepmc=%_prefix --with-rivet=%_prefix --with-fastjet=%_prefix  --with-lhapdf=/usr   --with-hepmcversion=3
make %{?_smp_mflags}
%endif

%install
%make_install 
export QA_RPATHS=3
mkdir -p %{buildroot}%{_sysconfdir}/ld.so.conf.d
%if %{?fedora}%{!?fedora:0}
echo %{_libdir}/%{name} >  %{buildroot}%{_sysconfdir}/ld.so.conf.d/%{name}-%{_arch}.conf
%endif


%files 
%doc AUTHORS README COPYING
%_bindir/*
%_datadir/ThePEG/*
%_libdir/ThePEG/*
%if %{?fedora}%{!?fedora:0}
%config(noreplace) %{_sysconfdir}/ld.so.conf.d/%{name}-%{_arch}.conf
%endif

%post -p /sbin/ldconfig
%postun -p /sbin/ldconfig


%files -n %{libnamedev}
%_includedir/ThePEG

%changelog
* Tue Jul 12 2022 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
  - 2.2.3
* Sun Aug 01 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
  - RPATH
* Fri Feb 19 2021 Andrii Verbytskyi 2.2.2
+ Bump to 2.2.2
* Thu May 26 2016 Andrii Verbytskyi 2.0.2
+ Initial spec file
