Name:        binder
Version:    1.4.1
Release:    1%{?dist}
Summary:    A tool for automatic generation of Python bindings to C++ code
License:    MIT License
URL:        https://github.com/RosettaCommons/binder
#Source0:    https://github.com/RosettaCommons/binder/archive/v{version}.tar.gz
Source0:    https://github.com/RosettaCommons/binder/archive/2d4b1aa19ca3b32b4b459e7a99048ae96712618b.zip

BuildRequires:    clang clang-devel llvm-devel 
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 9
BuildRequires:    clang-libs
Requires:         clang-libs
BuildRequires:    pybind11-devel
Requires:         pybind11-devel
%endif

%if 0%{?suse_version}
BuildRequires:    libclang-cpp17
Requires:         libclang-cpp17
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
%setup -q -n binder-2d4b1aa19ca3b32b4b459e7a99048ae96712618b

%build
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0}
%if %{?fedora}%{!?fedora:0} > 36 || %{?rhel}%{!?rhel:0} == 8
%if %{?fedora}%{!?fedora:0} > 37
sed -i 's/CMAKE_CXX_STANDARD 14/CMAKE_CXX_STANDARD 17/g' CMakeLists.txt
%endif
%cmake  -DBINDER_ENABLE_TEST=OFF
%else
%cmake . -DBINDER_ENABLE_TEST=OFF
%endif
%cmake_build
%endif

%if 0%{?suse_version}
cmake .  -DCMAKE_INSTALL_PREFIX=/usr -DBINDER_ENABLE_TEST=OFF
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
* Thu Dec 21 2023 Andrii Verbytskyi <andrii.verbtskyi@mpp.mpg.de>
+ 1.4.0
* Fri Mar 10 2023 Andrii Verbytskyi 
- Try to build on epel
* Wed Jan 19 2022  Andrii Verbytskyi 
+ Patch for more includes 
* Tue Dec 7 2021  Andrii Verbytskyi 
+ Version bump
* Fri Apr 9 2021  Andrii Verbytskyi 
+ Added debug and cleaned up. CentOS8+
* Thu Nov 5 2020 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> - 1.1.0-1
- Update to 1.1.0
* Fri Jan 3 2020 A V <andriish@mpp.mpg.de> - 0.9.9-3
- Init

