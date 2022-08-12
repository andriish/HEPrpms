#
# spec file for package qcdloop
#
# Copyright (c) 2020 SUSE LLC
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.

# Please submit bugfixes or comments via https://bugs.opensuse.org/
#

%global debug_package %{nil}
Name:           qcdloop
Version:        2.0.9
Release:        2
Summary:        An object-oriented one-loop scalar Feynman integrals framework
License:        GPL-3.0-only
URL:            https://qcdloop.web.cern.ch/qcdloop/
Source:         https://github.com/scarrazza/qcdloop/archive/%{version}.tar.gz
Patch0:         patch-qcdloop-0.txt
BuildRequires:  gcc-c++
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran
%endif
BuildRequires:  cmake >= 3.9
%if 0%{?suse_version}
BuildRequires:  gcc-fortran
%endif


%description
QCDLoop is a library of one-loop scalar Feynman integrals, evaluated close to
four dimensions. QCDLoop can compute one-loop integrals for tadpole, bubble,
triangle and box topologies. See  arXiv:0712.1851 and arXiv:1605.03181 for
references.

%package devel
Summary:        Development headers and sources for QCDLoop
Requires:       %{name} = %{version}-%{release}

%description devel
QCDLoop is a library of one-loop scalar Feynman integrals, evaluated close to
four dimensions. This package provides headers and sources for QCDLoop needed
for developing software against QCDLoop.

%prep -q -n %{name}-%{version}
%autosetup -n %{name}-%{version} -p1
sed -i "1{s|#! %{_bindir}/env bash|#! /bin/bash|}"  src/qcdloop-config.in


%build
%cmake \
  -DENABLE_EXAMPLES:BOOL=ON \
  -DENABLE_FORTRAN_WRAPPER:BOOL=ON

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} > 8
%cmake_build 
%else
%make_build 
%endif

%install
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} > 8
%cmake_install 
%endif
%if %{?rhel}%{!?rhel:0} 
%make_install 
%endif
%if 0%{?suse_version}
%cmake_install 
%endif


%post  -p /sbin/ldconfig
%postun  -p /sbin/ldconfig

%files
%{_libdir}/libqcdloop.so*

%files devel
%license LICENSE
%doc README.md
%{_bindir}/qcdloop-config
%{_libdir}/pkgconfig/*.pc
%{_includedir}/%{name}/

%changelog
* Fri Feb  19 2021 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de>
- Update to version 2.0.6
* Fri Aug  7 2020 Atri Bhattacharya <badshah400@gmail.com>
- Update to version 2.0.5:
  * Fixes gh#scarrazza/qcdloop#10.
* Sat Aug  1 2020 Atri Bhattacharya <badshah400@gmail.com>
- Add patches:
  * qcdloop-math-linking.patch: Explicitly link to math library to
    fix linking error when linking with --Wl,no-undefined
  * qcdloop-soversion.patch: Implement so versioning
  * qcdloop-fix-conflicting-types.patch: Explicitly cast a
    variable type to ensure consistency across build archs; fixes
    build failures for i586
  * qcdloop-remove-march-mtune-flags.patch: Drop march and mtune
    flags being passed to the c++ compiler to enable building on
    multiple archs.
* Wed Jul 22 2020 Atri Bhattacharya <badshah400@gmail.com>
- Initial package.
