#undefine _debugsource_packages
%define git_version 1215c9afb685d9ba44628c7b05a93b23ca240bb5
Name:           XNNPACK
Version:        1.0.0
Release:        1%{?dist}
Summary:        Library for C++

License:        BSD
URL:            https://github.com/google/XNNPACK/
Source0:        https://github.com/google/XNNPACK/archive/%{git_version}.tar.gz

BuildRequires:  gcc
BuildRequires:  gcc-c++
BuildRequires:  cmake FXDiv-devel FP16-devel psimd-devel pthreadpool pthreadpool-devel

%description
Some lib

%package        devel
Summary:        Development files for %{name}


%description    devel
This package contains the header file for using %{name}.


%prep
%setup -q -n %{name}-%{git_version}


%build
%cmake -DXNNPACK_BUILD_TESTS=OFF -DXNNPACK_BUILD_BENCHMARKS=OFF -DXNNPACK_USE_SYSTEM_LIBS=ON
%cmake_build

%install
%cmake_install

%files devel
%{_includedir}/*

%files
%{_libdir}/*


%changelog
* Fri Jan 26 2018 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 1.7.0
- start
