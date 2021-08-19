%undefine _debugsource_packages
%define git_version febbb1c163726b5db24bed55cc9dc42529068997
Name:           FP16
Version:        1.0.0
Release:        2%{?dist}
Summary:        Library for C++

License:        BSD
URL:            https://github.com/Maratyszcza/FP16
Source0:        https://github.com/Maratyszcza/FP16/archive/%{git_version}.tar.gz
Source1:        https://github.com/Maratyszcza/psimd/archive/refs/heads/master.zip

BuildRequires:  gcc
BuildRequires:  gcc-c++
BuildRequires:  cmake

%description
Some lib

%package        devel
Summary:        Development files for %{name}
Requires:       %{name}%{?_isa} = %{version}-%{release}


%description    devel
This package contains the header file for using %{name}.


%prep
%setup -q -n %{name}-%{git_version}
%setup -T -D -a 1 -n %{name}-%{git_version}


%build
%cmake  -DFP16_BUILD_TESTS:BOOL=OFF -DFP16_BUILD_BENCHMARKS:BOOL=OFF -DPSIMD_SOURCE_DIR=$(pwd)/psimd-master
%cmake_build

%install
%cmake_install

%files devel
%{_includedir}/*

%changelog
* Fri Jan 26 2018 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 1.7.0
- start
