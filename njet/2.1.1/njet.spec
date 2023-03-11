Name:           njet
Version:        2.1.1
Release:        2%{?dist}
License:        GPL
Url:            https://bitbucket.org/njet
Source0:        https://bitbucket.org/njet/njet/downloads/%{name}-%{version}.tar.gz
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
%setup -q
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
* Sun Feb 21 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Cleanup 
* Thu May 26 2016 Andrii Verbytskyi 2.0.0
+ Initial spec file
