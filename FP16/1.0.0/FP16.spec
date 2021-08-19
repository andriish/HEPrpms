%undefine _debugsource_packages
%define git_version febbb1c163726b5db24bed55cc9dc42529068997
Name:           FP16
Version:        1.0.0
Release:        1%{?dist}
Summary:        Library for C++

License:        BSD
URL:            https://github.com/Maratyszcza/FP16
Source0:        https://github.com/Maratyszcza/FP16/archive/%{git_version}.tar.gz

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
%cmake  -DFP16_BUILD_TESTS:BOOL=OFF -DFP16_BUILD_BENCHMARKS:BOOL=OFF
%cmake_build

%install
%cmake_install
#mkdir -p %{buildroot}/%{_libdir}/
#mv %{buildroot}/%{_prefix}/lib/* %{buildroot}/%{_libdir}/




%files devel
%{_includedir}/*
#{_prefix}/lib//cmake/{name}


%changelog
* Fri Jan 26 2018 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 1.7.0
- start
