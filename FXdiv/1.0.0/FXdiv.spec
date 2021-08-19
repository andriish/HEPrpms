%undefine _debugsource_packages
%define git_version b742d1143724d646cd0f914646f1240eacf5bd73
Name:           FXdiv
Version:        1.0.0
Release:        3%{?dist}
Summary:        Library for C++

License:        BSD
URL:            https://github.com/Maratyszcza/FXdiv
Source0:        https://github.com/Maratyszcza/FXdiv/archive/%{git_version}.tar.gz


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
%cmake  -DFXDIV_BUILD_TESTS=OFF  -DFXDIV_BUILD_BENCHMARKS=OFF 
%cmake_build

%install
%cmake_install

%files devel
%{_includedir}/*

%changelog
* Fri Jan 26 2018 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 1.7.0
- start
