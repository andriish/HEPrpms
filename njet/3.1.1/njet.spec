Name:           njet
Version:        3.1.1
Release:        1%{?dist}
License:        GPL
Url:            https://bitbucket.org/njet
Source0:        https://bitbucket.org/njet/njet/get/d63d1068374296ba13a1d81877983e4a37c8fc44.tar.gz
Summary:        A library for multi-parton one-loop matrix elements
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran gcc-c++  autoconf automake libtool  qd qd-devel
Requires:       python3 
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
sed -i 's@python@python3@1'  blha/njet.py

%build
export FFLAGS="%{optflags} -std=legacy"
%configure --with-qd=/usr 
make  
%install
%make_install
find %{buildroot} -name '*.la' -delete


%files 
%_libdir/pkgconfig/*
%_libdir/*.*
%_bindir/*
%_includedir/*
/usr/share/*


%changelog
* Fri Mar 10 2023 Andrii Verbytskyi 3.1.1
- Update to 3.1.1
* Sun Feb 21 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Cleanup 
* Thu May 26 2016 Andrii Verbytskyi 2.0.0
+ Initial spec file
