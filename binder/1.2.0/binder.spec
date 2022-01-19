Name:        binder
Version:    1.2.0
Release:    2%{?dist}
Summary:    A tool for automatic generation of Python bindings to C++ code
License:    MIT License
URL:        https://github.com/RosettaCommons/binder
Source0:    https://github.com/RosettaCommons/binder/archive/v%{version}.tar.gz
Patch0:         patch-binder-0.txt

BuildRequires:    clang clang-devel llvm-devel 
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0}
BuildRequires:    clang-libs
Requires:         clang-libs
BuildRequires:    pybind11-devel
Requires:         pybind11-devel
%endif

%if 0%{?suse_version}
BuildRequires:    libclang11
Requires:         libclang11
BuildRequires:    python-pybind11-common-devel
Requires:         python-pybind11-common-devel
%endif
BuildRequires:    python3 python3-devel
BuildRequires:    gcc-c++ unzip
BuildRequires:    cmake >= 3.4.3
%description
Binder is a tool for automatic generation of Python bindings for C++11 
projects using Pybind11 and Clang LibTooling libraries. That is, Binder, 
takes a C++ project and compiles it into objects and functions that 
are all usable within Python. Binder is different from prior tools in 
that it handles special features new in C++11.

%prep
%setup -q
%patch0 -p1

%build
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0}
%cmake . -DBINDER_ENABLE_TEST=OFF
%cmake_build
%endif

%if 0%{?suse_version}
cmake .  -DCMAKE_INSTALL_PREFIX=/usr
make
%endif

%install
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0}
%cmake_install
%endif
%if 0%{?suse_version}
%make_install
%endif



%files
%{_bindir}/*

%changelog
* Wed Jan 19  Andrii Verbytskyi 
+ Patch for more includes 
* Tue Dec 7 2021  Andrii Verbytskyi 
+ Version bump
* Fri Apr 9 2021  Andrii Verbytskyi 
+ Added debug and cleaned up. CentOS8+
* Thu Nov 5 2020 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> - 1.1.0-1
- Update to 1.1.0
* Fri Jan 3 2020 A V <andriish@mpp.mpg.de> - 0.9.9-3
- Init

