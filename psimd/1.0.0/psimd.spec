%undefine _debugsource_packages
%define git_version 10b4ffc6ea9e2e11668f86969586f88bc82aaefa
Name:           psimd
Version:        1.0.0
Release:        2%{?dist}
Summary:        Library for C++

License:        BSD
URL:            https://github.com/Maratyszcza/psimd
Source0:        https://github.com/Maratyszcza/psimd/archive/%{git_version}.tar.gz


BuildRequires:  gcc
BuildRequires:  gcc-c++
BuildRequires:  cmake

%description
Some lib

%package        devel
Summary:        Development files for %{name}



%description    devel
This package contains the header file for using %{name}.


%prep
%setup -q -n %{name}-%{git_version}


%build
#sed -i '/PROJECT(FP16 C CXX)/a add_library(psimd INTERFACE)' CMakeLists.txt
%cmake  -DSIMD_BUILD_TESTS:BOOL=OFF -DSIMD_BUILD_BENCHMARKS:BOOL=OFF 
%cmake_build

%install
%cmake_install

%files devel
%{_includedir}/*

%changelog
* Fri Jan 26 2018 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 1.7.0
- start
