Name:           qqvvamp
Version:        1.1
Release:        1%{?dist}
Summary:        qqvvamp package from vvamp.hepforge.org

License:        GPL
URL:            https://vvamp.hepforge.org/
Source0:        http://www.hepforge.org/archive/vvamp/%{name}-%{version}.tar.gz
Patch0:         patch-0.txt

BuildRequires:  cmake
BuildRequires:  gcc
BuildRequires:  ginac-devel
BuildRequires:  cnl-devel

Requires:       ginac
Requires:       cnl

%description
qqvvamp is a package for symbolic computation and numerical analysis.

%package devel
Summary:        Development files for qqvvamp
Requires:       %{name} = %{version}-%{release}
Requires:       ginac-devel
Requires:       cnl-devel

%description devel
This package contains the header files and libraries needed to develop applications that use qqvvamp.

%prep
%setup -q
%patch0 -p1
sed -i 's/lst/LST/g' p0002.cpp

%build
%cmake .
%cmake_build

%install
%cmake_install

%files
%license LICENSE
%doc README.md
%{_libdir}/libqqvvamp.so

%files devel
%{_includedir}/qqvvamp.h

%changelog
* Thu Oct 24 2024 Your Name <you@example.com> - 1.0.0-1
•  Initial package with patch and devel subpackage