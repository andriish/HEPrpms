%global debug_package %{nil}
%global _lto_cflags %nil
Name:           qqvvamp
Version:        1.1
Release:        3%{?dist}
Summary:        qqvvamp package from vvamp.hepforge.org

License:        GPL
URL:            https://vvamp.hepforge.org/
Source0:        https://gitlab.cern.ch/averbyts/qqvvamp/-/archive/master/qqvvamp-master.tar.gz
Patch0:         patch-0.txt

BuildRequires:  cmake
BuildRequires:  gcc-c++
BuildRequires:  ginac-devel
BuildRequires:  cln-devel
BuildRequires:  libquadmath-devel

Requires:       ginac
Requires:       cln

%description
qqvvamp is a package for symbolic computation and numerical analysis.

%package devel
Summary:        Development files for qqvvamp
Requires:       %{name} = %{version}-%{release}
Requires:       ginac-devel
Requires:       cln-devel
BuildRequires:  libquadmath-devel

%description devel
This package contains the header files and libraries needed to develop applications that use qqvvamp.

%prep
%setup -q -n qqvvamp-master
%patch -P 0 -p1

%build
%if %{?rhel}%{!?rhel:0} == 9
export CXXFLAGS='-O0 -g0 -fno-var-tracking -mcmodel=medium -fdata-sections -ffunction-sections -fno-implicit-templates'
%else
export CXXFLAGS='-O1 -fno-var-tracking -mcmodel=medium -fdata-sections -ffunction-sections'
%endif

export LDFLAGS='-Wl,--as-needed'
%cmake .
%if %{?rhel}%{!?rhel:0} == 9
export CXXFLAGS='-O0 -fno-var-tracking -mcmodel=medium -fdata-sections -ffunction-sections -fno-implicit-templates'
%cmake_build -j4
%endif
%cmake_build

%install
%cmake_install

%files
%{_libdir}/libqqvvamp*

%files devel
%{_includedir}/qq*
%{_datadir}/cmake/qqvvamp

%changelog
* Thu Oct 24 2024 Your Name <you@example.com> - 1.0.0-1
•  Initial package with patch and devel subpackage
