%define major       0
%define libname     MC-TESTER
%define libnamedev  MC-TESTER-devel


Name:           MC-TESTER
Version:        1.25.0
Release:        2%{?dist}
License:        Unknown
Url:            http://mc-tester.web.cern.ch/MC-TESTER/
Source0:        http://mc-tester.web.cern.ch/MC-TESTER/MC-TESTER-1.25.0.tar.gz
#HepMC3 support
Patch0:         patch-MC-TESTER-0.txt
Summary:        a universal tool for comparisons of Monte Carlo predictions in High Energy Physics

BuildRequires:  autoconf automake libtool  doxygen
%if 0%{?rhel} || 0%{?fedora}
Requires:       HepMC  HepMC3 root  root-core root-graf  root-hist
BuildRequires:  HepMC HepMC-devel HepMC3 HepMC3-devel root  root-core root-graf  root-hist
%endif
%if 0%{?suse_version}
Requires:       libHepMC4 libHepMC3-1  root6  root6-libs root6-devel
BuildRequires:  libHepMC4 libHepMC3-1  root6  root6-libs root6-devel HepMC2-devel HepMC3-devel
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
%setup -q -n %{name}
%patch0 -p1


%build
autoreconf
mkdir -p lib
%configure  --with-HepMC=%_prefix --with-HepMC3=%_prefix   --with-root=%_prefix  
make %{?_smp_mflags}
make -C doc

%install
%make_install 
%files   -n %{libname}
%_libdir/*

%files -n %{libnamedev}
%_includedir/*

%changelog
* Wed Apr 21 2021 Andrii Verbytskyi
+ Cleanup
* Thu May 26 2016 Andrii Verbytskyi
+ Initial spec file

