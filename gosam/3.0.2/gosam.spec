%define major            0
%define libname          gosam
%define libnamedev       gosam-devel
%define develnamestatic  gosam-static-devel
%define __os_install_post %{nil}
%undefine _debugsource_packages

Name:           gosam
Version:        3.0.2
Release:        1%{?dist}
License:        GPLv3
Url:            https://github.com/gudrunhe/gosam
Source0:        https://github.com/gudrunhe/gosam/archive/refs/tags/v%{version}.tar.gz
Source1:        https://bitbucket.org/hameren/oneloop/get/4b012d009f22f34d5be6d2d98600e4bad6c185fd.tar.gz
Patch0:         patch-gosam-0.txt
Summary:        Automated calculation of one-loop amplitudes 
Requires:       form qgraf

BuildRequires:  meson ninja
BuildRequires:  gcc-gfortran
BuildRequires:  python3 python3-devel

%if 0%{?rhel} || 0%{?fedora}
BuildRequires: python3-setuptools
%endif
%if 0%{?suse_version}
BuildRequires: gcc-fortran python-rpm-macros python3-setuptools
%endif

%description
The package GoSam allows for the automated calculation of one-loop 
amplitudes for multi-particle processes in renormalizable quantum 
field theories. The amplitudes, which are generated in terms of Feynman 
diagrams, can be reduced using either D-dimensional integrand-level
decomposition or by tensor reduction. GoSam can be used to calculate 
one-loop QCD and/or electroweak corrections to Standard Model processes
and offers the flexibility to link model files for theories Beyond the 
Standard Model. A standard interface (BLHA1, BLHA2) to programs 
calculating real radiation is also implemented.

%prep
%setup -q -n gosam-%{version}
%patch -P 0 -p1
#cp {SOURCE1} subprojects/packagefiles/

%build
%meson --wrap-mode=default -Dform=false -Dqgraf=false
%meson_build

%install
%meson_install

mkdir -p %{buildroot}/%{_bindir}
mv %{buildroot}/%{_prefix}/gosam.py %{buildroot}/%{_bindir}/gosam.py
rm -rf %{buildroot}/%{_bindir}/gosam_setup_env.sh

%files -n %{libname}
%{_bindir}/gosam.py
%{python3_sitelib}/golem/
%{_libdir}/libavh_olo.a  
%{_libdir}/libavh_olo.so
%{_libdir}/libninja.a
%{_libdir}/libninja.so
%{_libdir}/pkgconfig/avh_olo.pc  
%{_libdir}/pkgconfig/ninja.pc
%{_datadir}/models/
%{_includedir}/avh_olo  
%{_includedir}/ninja
%{_includedir}/quadninja
%{_datadir}/olp/
%{_datadir}/src/
%{_datadir}/templates/

%changelog
* Fri Sep 12 2025 Andrii Verbytskyi <andrii.verbytskyi@cern.ch> - 3.0.2-1
- Switched to Meson build system
- Updated build dependencies and macros
* Mon Dec 30 2024 Andrii Verbytskyi <andrii.verbytskyi@cern.ch> - 2.1.2
- Update to 2.1.2
* Mon Nov 15 2021 Andrii Verbytskyi <andrii.verbytskyi@cern.ch> - 2.1.1
- Bump to 2.1.1
* Fri Feb 19 2021 Andrii Verbytskyi <andrii.verbytskyi@cern.ch> - 2.1.0
+ New version
* Thu May 26 2016 Andrii Verbytskyi <andrii.verbytskyi@cern.ch> - 2.0.3
+ Initial spec file
