%define debug_package %{nil}
Name:           FeynHiggs
Version:        2.18.0
Release:        1%{?dist}
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
%patch0 -p1


%build
#Flags are not propagated
./configure \
--prefix=%{prefix} --libdir=%{_libdir} \
--quad \
--64 \
--native \
--enable-full-g-2 \
--enable-slhapara  CXXFLAGS="%{optflags}  -fPIC" FFLAGS="%{optflags}  -fPIC" CFLAGS="%{optflags}  -fPIC"  LDFLAGS="%{build_ldflags}"

%make_build

%install
%make_install
rm -f  $RPM_BUILD_ROOT/%{_bindir}/fcc


%files 
# This is a duplicate of LopTools {_bindir}/fcc
%{_bindir}/FeynHiggs
%{_bindir}/table


%files devel
%{_libdir}/libFH.a
%{_includedir}/*.h

%changelog
* Sat Mar 30 2019 umeabot <umeabot> 4.2-6.mga7
+ xxxxxxx
