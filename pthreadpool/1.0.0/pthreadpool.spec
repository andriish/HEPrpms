%undefine _debugsource_packages
%define git_version d465747660ecf9ebbaddf8c3db37e4a13d0c9103

Name:           pthreadpool
Version:        1.0.0
Release:        1%{?dist}
Summary:        Library for C++

License:        BSD
URL:            https://github.com/Maratyszcza/pthreadpool
Source0:        https://github.com/Maratyszcza/pthreadpool/archive/%{git_version}.tar.gz

BuildRequires:  gcc FXdiv-devel
Requires: FXdiv-devel
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
sed -i '/PROJECT(pthreadpool C CXX)/a add_library(fxdiv INTERFACE)' CMakeLists.txt
%cmake  -DPTHREADPOOL_BUILD_TESTS=OFF -DPTHREADPOOL_BUILD_BENCHMARKS=OFF  -DCMAKE_INSTALL_LIBDIR=%{_libdir}   -DFXDIV_SOURCE_DIR=/foo
%cmake_build

%install
%cmake_install

%files 
%{_libdir}/*

%files devel
%{_includedir}/*

%changelog
* Fri Jan 26 2018 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 1.7.0
- start
