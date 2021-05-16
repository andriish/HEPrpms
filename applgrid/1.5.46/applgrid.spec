Name:           applgrid
Version:        1.5.46
Release:        3%{?dist}
License:        GPL
Prefix:         %{_prefix}
Summary:        A fast and flexible way to reproduce the results of full NLO calculations with any input parton distribution 
Source:         http://www.hepforge.org/archive/applgrid/applgrid-%{version}.tgz
Patch0:         patch-applgrid-0.txt
URL:            https://applgrid.hepforge.org/
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-c++ gcc-gfortran hoppet root   autoconf binutils automake libtool 
Requires:       hoppet root
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-c++ gcc-fortran hoppet root6 root6-libs root6-devel  autoconf binutils automake libtool 
Requires:       hoppet root6 root6-libs root6-devel libgfortran5
%endif

%description
The APPLgrid project provides a fast and flexible way to reproduce the results 
of full NLO calculations with any input parton distribution set in only a few 
milliseconds rather than the weeks normally required to gain adequate statistics.
Written in C++ (although a fortran interface is included) it can be used for the 
calculation of any process where the hard subprocess weights from the convolution 
with the PDF are available from the calculation.

%prep 
%setup -q 
%patch0 -p1

%build 
autoreconf --force --install --verbose .


%configure
%make_build

%install 
%make_install

%files
%defattr(-,root,root)
%{_bindir}/*
%{_libdir}/*
%{_includedir}/*


%post 
ldconfig 

%changelog
* Tue Apr 20 2021 Andrii Verbytskyi <andrii.verbtskyi@mpp.mpg.de>
+ Preparation for release
* Sat Apr 03 2021  Andrii Verbytskyi 
+ Cleanup.
* Fri Nov 29 2019 Andrii Verbytskyi 1.5.40
 - Initial
