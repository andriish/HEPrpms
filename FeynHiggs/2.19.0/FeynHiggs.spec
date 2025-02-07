%define debug_package %{nil}
Name:           FeynHiggs
Version:        2.19.0
Release:        4%{?dist}
Summary:        Fortran code for the (diagrammatic/EFT/hybrid) calculation of the masses, mixings and much more of the Higgs bosons
License:        GPL
Prefix:         %{_prefix}
URL:            https://wwwth.mpp.mpg.de/members/heinemey/feynhiggs/
Source:         https://wwwth.mpp.mpg.de/members/heinemey/feynhiggs/newversion/%{name}-%{version}.tar.gz
Patch0:         patch-FeynHiggs-0.txt


%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-c++ 
BuildRequires:  gcc-gfortran
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-c++ 
BuildRequires:  gcc-fortran
%endif



%description 
FeynHiggs is a Fortran code for the (diagrammatic/EFT/hybrid) calculation of the masses, 
mixings and much more of the Higgs bosons in the MSSM with real/complex parameters 
at the highest level of accuracy. 

%package devel
License:        GPL
Summary:        Headers and modules for the FeynHiggs
Provides:       %{name}-devel = %{version}-%{release}

%description devel
The Headers and modules for the FeynHiggs


%prep

%setup -q
%patch -P 0 -p1


%build
#Flags are not propagated
export CC=gcc
MYOPTFLAGS=$(echo %{optflags} | sed -e 's/-flto//g' -e 's/-fuse-linker-plugin//g' -e 's/-ffat-lto-objects//g')
MYLDFLAGS=$(echo %{build_ldflags} | sed -e 's/-flto//g' -e 's/-fuse-linker-plugin//g' -e 's/-ffat-lto-objects//g')
./configure \
--quad \
--64 \
--native \
--enable-full-g-2 \
--enable-slhapara  CXXFLAGS="$MYOPTFLAGS -fPIC" FFLAGS="$MYOPTFLAGS  -fPIC -Wno-tabs" CFLAGS="$MYOPTFLAGS -fPIC"  LDFLAGS="$MYLDFLAGS"

%make_build

%install
%make_install
rm -f  $RPM_BUILD_ROOT/%{_bindir}/fcc


%files 
# This is a duplicate of LoopTools {_bindir}/fcc
%{_bindir}/FeynHiggs
%{_bindir}/table


%files devel
%{_libdir}/libFH.a
%{_includedir}/*.h

%changelog
* Tue Nov 15 2022 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 2.19
- Version 2.19
