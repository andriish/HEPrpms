%undefine _debugsource_packages
%define git_version 97fe555430a857581b9b826ecd955e4f0a3653f0
Name:           foxi
Version:        1.0.0
Release:        1%{?dist}
Summary:        Library for C++

License:        BSD
URL:            https://github.com/houseroad/foxi
Source0:        https://github.com/houseroad/foxi/archive/%{git_version}.tar.gz


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
%cmake   
%cmake_build

%install
%cmake_install

%files devel
%{_includedir}/*

%changelog
* Fri Jan 26 2018 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 1.7.0
- start
