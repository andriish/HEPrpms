%undefine _debugsource_packages
%define git_version 0e6bde92b343c5fbcfe34ecd41abf9515d54b4a7

Name:           cpuinfo
Version:        1.0.0
Release:        1%{?dist}
Summary:        Library for C++

License:        BSD
URL:            https://github.com/pytorch/cpuinfo
Source0:        https://github.com/pytorch/cpuinfo/archive/%{git_version}.tar.gz

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


%build
%cmake  -DCPUINFO_BUILD_UNIT_TESTS=OFF -DCPUINFO_BUILD_MOCK_TESTS=OFF  -DCPUINFO_BUILD_BENCHMARKS=OFF  -DCMAKE_INSTALL_LIBDIR=%{_libdir}
%cmake_build

%install
%cmake_install

%files 
%{_bindir}/*
%{_libdir}/*

%files devel
%{_includedir}/*

%changelog
* Fri Jan 26 2018 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 1.7.0
- start
