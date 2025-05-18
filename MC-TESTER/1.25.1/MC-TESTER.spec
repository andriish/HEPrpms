Name:           MC-TESTER
Version:        1.25.1
Release:        17%{?dist}
License:        MIT
Url:            http://mc-tester.web.cern.ch/MC-TESTER/
Source0:        https://gitlab.cern.ch/cvsmctst/mc-tester/-/archive/v1.25.1/mc-tester-v%{version}.tar.gz
Patch0:         patch-MC-TESTER-0.txt
Summary:        A universal tool for comparisons of Monte Carlo predictions in High Energy Physics

BuildRequires:  autoconf automake libtool  doxygen
BuildRequires:  gcc-c++ make
%if 0%{?rhel} || 0%{?fedora}
Requires:       HepMC  HepMC3 root  root-core root-graf  root-hist
BuildRequires:  HepMC HepMC-devel HepMC3 HepMC3-devel root  root-core root-graf  root-hist
%endif
%if 0%{?suse_version}
Requires:       libHepMC4 libHepMC3-4  root6-config  root6-libs root6-devel root6
BuildRequires:  libHepMC4 libHepMC3-4  root6-config  root6-libs root6-devel root6 HepMC2-devel HepMC3-devel
BuildRequires:  unzip
%endif

Prefix: %{_prefix}

%description
 MC-TESTER is a toolkit for providing a common platform for using and 
 building event generators in C++. 

%package  devel
Summary:        Libraries and headers for %{name}
Provides:       %{name}-devel = %{version}-%{release}

%description devel
%{libnamedev} contains the libraries and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.




%prep
%setup -q -n mc-tester-v%{version}
%patch -P 0 -p1


%build
cd include 
ln -s ../HepMC3Event/HepMC3Particle.h 
ln -s ../HepMC3Event/HepMC3Event.h 
cd ..
autoreconf
mkdir -p lib
%configure  --with-HepMC=%_prefix --with-HepMC3=%_prefix   --with-root=%_prefix  
make %{?_smp_mflags}
make -C doc


%install
%make_install
%if 0%{?suse_version}
mkdir -p $RPM_BUILD_ROOT/usr/share/doc/packages
mv $RPM_BUILD_ROOT/usr/share/doc/MC-TESTER $RPM_BUILD_ROOT/usr/share/doc/packages
%endif

%files
%_libdir/*

%files devel
%_includedir/*
%_docdir/MC-TESTER/*

%changelog
* Tue Jun 22 2021 Andrii Verbytskyi
+ Added dependency on gcc
* Thu May 06 2021 Andrii Verbytskyi
+ Version 1.25.1
* Wed Apr 21 2021 Andrii Verbytskyi
+ Cleanup
* Thu May 26 2016 Andrii Verbytskyi
+ Initial spec file

