%global debug_package %{nil}
%global _lto_cflags %nil
Name:           nlox
Version:        1.2.1
Release:        9%{?dist}
Summary:        nlox package 

License:        GPL
URL:            http://www.hep.fsu.edu/~nlox/
Source0:        https://gitlab.cern.ch/averbyts/nlox/-/archive/1.2.2.atlas7/nlox-1.2.2.atlas7.zip
BuildRequires:  cmake
BuildRequires:  git
BuildRequires:  gcc-gfortran
BuildRequires:  gcc-c++
BuildRequires:  python3
BuildRequires:  libquadmath-devel
BuildRequires:  qcdloop-devel



%description
nlox is a package .


%prep
%setup -q -n nlox-1.2.2.atlas7

%build
export CXXFLAGS='%{optflags} -fno-var-tracking -Wno-reorder -Wno-sign-compare -Wno-unused-variable -std=c++17 -fext-numeric-literals'
sed -i 's|URL http://helac-phegas.web.cern.ch/helac-phegas/tar-files/OneLOop-3.6.tgz|URL http://madgraph.phys.ucl.ac.be/Downloads/OneLOop-3.6.tgz|g' CMakeLists.txt
sed -i 's|https://github.com/andriish/qcdloop|https://github.com/scarrazza/qcdloop|g' CMakeLists.txt
sed -i 's|GIT_TAG arm|GIT_TAG 2\.1\.0|g' CMakeLists.txt
%if  %{?rhel}%{!?rhel:0} == 8
 sed -i 's/-fallow-argument-mismatch//g' CMakeLists.txt
%endif
%cmake .   -DCMAKE_CXX_STANDARD=17 -DCMAKE_INSTALL_LIBDIR:PATH=%{_libdir}/nlox  -DDEFAULT_NLOX_PATH=%{_libdir}/nlox -DCMAKE_VERBOSE_MAKEFILE=OFF -DNLOX_PROCESSES="pp_Wpttbar;pp_Wmttbar;pp_Zttbar_as3ae1;pp_ttbarepem_as3ae2"
%cmake_build

%install
%cmake_install

%files
%{_libdir}/nlox


%changelog
* Thu Oct 24 2024 Your Name <you@example.com> - 1.0.0-1
•  Initial package with patch and devel subpackage
