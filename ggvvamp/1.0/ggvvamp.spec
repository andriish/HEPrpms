Name:           ggvvamp
Version:        1.0
Release:        1%{?dist}
Summary:        ggvvamp package from vvamp.hepforge.org

License:        GPL
URL:            https://vvamp.hepforge.org/
Source0:        https://www.hepforge.org/archive/vvamp/%{name}-%{version}.tar.gz
Patch0:         patch-0.txt

BuildRequires:  cmake
BuildRequires:  gcc
BuildRequires:  ginac-devel
BuildRequires:  cln-devel

Requires:       ginac
Requires:       cln

%description
ggvvamp is a package for symbolic computation and numerical analysis.

%package devel
Summary:        Development files for ggvvamp
Requires:       %{name} = %{version}-%{release}
Requires:       ginac-devel
Requires:       cln-devel

%description devel
This package contains the header files and libraries needed to develop applications that use ggvvamp.

%prep
%setup -q
%patch0 -p1

%build
%cmake .
%cmake_build

%install
%cmake_install

%files
%license LICENSE
%doc README.md
%{_libdir}/libggvvamp.so

%files devel
%{_includedir}/ggvvamp.h
%{_datadir}/cmake/ggvvamp

%changelog
* Thu Oct 24 2024 Your Name <you@example.com> - 1.0.0-1
•  Initial package with patch and devel subpackage