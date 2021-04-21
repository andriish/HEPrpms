%define libname     blackhat
%define libnamedev  blackhat-devel
%define libnamedata  blackhat-data


Name:           blackhat
Version:        0.9.9
Release:        2%{?dist}
License:        Unknown
Url:            http://www.blackhat.hepforge.org
Source0:        http://www.hepforge.org/archive/blackhat/%{name}-%{version}.tar.gz
Patch0:         patch-blackhat-0.txt
Patch1:         patch-blackhat-1.txt
Patch2:         patch-blackhat-2.txt
Patch3:         patch-blackhat-3.txt
Prefix:         %{_prefix}
Summary:        Automated calculation of one-loop amplitudes 

%if 0%{?rhel} || 0%{?fedora}
BuildRequires: autoconf automake libtool openssl-devel openssl
BuildRequires:  qd qd-devel swig gcc-c++
Requires:       qd openssl
%endif
%if 0%{?suse_version}
BuildRequires:  autoconf automake libtool openssl-devel openssl
BuildRequires:  libqd0 qd-devel swig gcc-c++   mpfr-devel gmp-devel
Requires:       libqd0 openssl
%endif

%description
BlackHat is an automated C++ program for calculating one-loop amplitudes. 
The program implements the unitarity method and on-shell recursion to 
construct amplitudes. As input to the calculation, it uses compact 
analytic formulae for tree amplitudes for four-dimensional helicity
 states. The program performs all related computations numerically. 
 It makes use of recently developed on-shell methods for evaluating 
 coefficients of loop integrals, introducing a discrete Fourier 
 projection as a means of improving efficiency and numerical stability.
  

%package  devel
Summary:        Libraries and headers for %{name}
Requires:       %{libname} = %{version}
Provides:       %{name}-devel = %{version}-%{release}

%description devel
%{libnamedev} contains the libraries and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.



%package    data
Summary:    Blackhat files

%description data
This package contains the headers required for compiling software that uses
the %{name} library


%prep
%setup -q
%patch0 -p1
%patch1 -p1
%patch2 -p1
%if 0%{?suse_version}
%patch3 -p1
%endif

%build
autoreconf --force --install --verbose 
%configure --enable-dependency-tracking 

make %{?_smp_mflags}

%install
%make_install



%files -n %{libname}
%{_bindir}/%{name}-config
%{_bindir}/dataInstall
%{_bindir}/LH_reader
%{_libdir}/blackhat/lib*

%files -n %{libnamedev}
%{_includedir}/blackhat/*.h


%files -n %{libnamedata}
%{_datadir}/blackhat/assembly/*
%{_datadir}/blackhat/datafiles/cut/4/*
%{_datadir}/blackhat/datafiles/cut/5/*
%{_datadir}/blackhat/datafiles/cut/6/*
%{_datadir}/blackhat/datafiles/cut/7/*
%{_datadir}/blackhat/datafiles/rat/4/*
%{_datadir}/blackhat/datafiles/rat/5/*
%{_datadir}/blackhat/datafiles/rat/6/*
%{_datadir}/blackhat/datafiles/rat/7/*
%{_datadir}/blackhat/datafiles/trees/4/*
%{_datadir}/blackhat/datafiles/trees/5/*
%{_datadir}/blackhat/datafiles/trees/6/*
%{_datadir}/blackhat/datafiles/trees/7/*
%{_datadir}/blackhat/examples/*
%{_datadir}/blackhat/parents/*
%{_datadir}/blackhat/svn*


%changelog
* Tue Apr 20 2021 Andrii Verbytskyi <andrii.verbtskyi@mpp.mpg.de>
+ Preparation for release
* Fri Nov 29 2019 Andrii Verbytskyi 0.9.9
+ Cleanup
* Thu May 26 2016 Andrii Verbytskyi 0.9.9
+ Initial spec file
