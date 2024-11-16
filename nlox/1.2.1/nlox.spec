%global debug_package %{nil}
%global _lto_cflags %nil
Name:           nlox
Version:        1.2.1
Release:        2%{?dist}
Summary:        nlox package 

License:        GPL
URL:            http://www.hep.fsu.edu/~nlox/
Source0:        https://gitlab.cern.ch/averbyts/nlox/-/archive/1.2.2.atlas3/nlox-1.2.2.atlas3.zip
BuildRequires:  cmake
BuildRequires:  gcc-gfortran
BuildRequires:  gcc-c++
BuildRequires:  python3
BuildRequires:  libquadmath-devel



%description
nlox is a package .


%prep
%setup -q -n nlox-1.2.2.atlas3

%build
export CXXFLAGS='%{optflags} -fno-var-tracking -Wno-reorder -Wno-sign-compare -Wno-unused-variable'
%cmake .  -DCMAKE_INSTALL_LIBDIR:PATH=%{_libdir}/nlox -DNLOX_PROCPATHFINAL=%{_libdir}/nlox -DCMAKE_VERBOSE_MAKEFILE=OFF -DNLOX_PROCESSES="pp_Wpttbar;pp_Wmttbar;pp_Zttbar_as3ae1;pp_ttbarepem_as3ae2"
%cmake_build

%install
%cmake_install

%files
%{_libdir}/nlox


%changelog
* Thu Oct 24 2024 Your Name <you@example.com> - 1.0.0-1
•  Initial package with patch and devel subpackage