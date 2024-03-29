Name:           njet
Version:        3.1.1
Release:        1%{?dist}
License:        GPL
Url:            https://bitbucket.org/njet
Source0:        https://bitbucket.org/njet/njet/get/d63d1068374296ba13a1d81877983e4a37c8fc44.tar.gz
Patch0:     patch-njet-0.txt
Summary:        A library for multi-parton one-loop matrix elements
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran gcc-c++  autoconf automake libtool  qd qd-devel
Requires:       python3 python3-devel
%endif
%if  %{?rhel}%{!?rhel:0} >= 8
BuildRequires: platform-python-devel
%endif

%if %{?fedora}%{!?fedora:0} 
BuildRequires: python-rpm-macros
%endif
%if %{?fedora}%{!?fedora:0} >= 35
BuildRequires: python-setuptools
%endif

%if 0%{?suse_version}
BuildRequires:  gcc-fortran gcc-c++  autoconf automake libtool  libqd0 qd-devel
Requires:       python 
%endif

Prefix: %{_prefix}

%description
NJet is a library for multi-parton one-loop matrix elements in 
massless QCD. NJet is based on the generalized unitarity program 
NGluon and uses QCDLoop/FF for scalar intergrals and libqd for 
extended precision arithmetic.

%prep
%setup -q -n njet-njet-d63d10683742
#sed -i 's@python@python3@1'  blha/njet.py
#patch0 -p1

%build
export FFLAGS="%{optflags} -std=legacy"
autoreconf -fi

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
pathfix.py -pn -i %{__python3}  ./
pathfix.py -pn -i %{__python3}  blha/*
pathfix.py -pn -i %{__python3}  examples/*
%endif

%configure --with-qd=/usr --with-oneloop
make  
%install
%make_install
find %{buildroot} -name '*.la' -delete


%files 
%_libdir/pkgconfig/*
%_libdir/*.*
%_bindir/*
%_includedir/*


%changelog
* Fri Mar 10 2023 Andrii Verbytskyi 3.1.1
- Update to 3.1.1
* Sun Feb 21 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Cleanup 
* Thu May 26 2016 Andrii Verbytskyi 2.0.0
+ Initial spec file
