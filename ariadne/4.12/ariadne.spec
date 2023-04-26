Name:          ariadne
Version:       4.12
Release:       7%{?dist}
Summary:       Multipurpose Monte Carlo Event Generator for High Energy physics.
License:       Public domain as stated by Leif 
URL:           http://home.thep.lu.se/~leif/ariadne
Source0:       https://home.thep.lu.se/~leif/ariadne/ariadne-4.12.tgz
Patch0:        patch-ariadne-0.txt
Prefix:        %{_prefix}
%if 0%{?rhel} || 0%{?fedora}
BuildRequires: gcc-gfortran
Requires:      libgfortran
%endif
%if 0%{?suse_version}
BuildRequires: gcc-fortran
Requires:      libgfortran5
%endif

%description
A Program for Simulation of QCD-Cascades Implementing the Colour Dipole Model


%prep
%setup -q
%patch0 -p1

%build
make %{?_smp_mflags}  libar4.so "FFLAGS=%{optflags} -fPIC" "LDFLAGS=%{build_ldflags}"
make %{?_smp_mflags}  libar4p5.so "FFLAGS=%{optflags} -fPIC" "LDFLAGS=%{build_ldflags}"

%install
make install INSTALLDIR=%{buildroot}%_prefix


%clean

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig


%files
%_libdir/*
%_includedir/ariadne/*

%changelog
* Tue Apr 20 2021 Andrii Verbytskyi <andrii.verbtskyi@mpp.mpg.de>
+ Preparation for release
* Sun Feb 21 2021 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de>
 - Cleanup 
* Fri May 24 2019 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> - 4.12
 - Update 
