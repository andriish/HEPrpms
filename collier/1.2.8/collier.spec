%undefine _debugsource_packages
Summary:  A Fortran library for the numerical evaluation of one-loop scalar and tensor integrals 
Name: collier
Version:  1.2.8
Release:  1%{?dist}
License:  GPLv3
Prefix: %{_prefix}
URL:      https://collier.hepforge.org/
Source:   https://www.hepforge.org/archive/collier/collier-%{version}.tar.gz
Requires: fastjet fastjet-devel hoppet
BuildRequires: fastjet-devel hoppet gcc-c++
BuildRequires: cmake >= 3.4.3
%if 0%{?rhel} || 0%{?fedora}
BuildRequires: gcc-gfortran
%endif
%if 0%{?suse_version}
BuildRequires: gcc-fortran
%endif

%description
 COLLIER is a fortran library for the numerical evaluation of one-loop 
 scalar and tensor integrals appearing in perturbative relativistic 
 quantum field theory with the following features: scalar and tensor 
 integrals for high particle multiplicities  dimensional regularization 
 for ultraviolet divergences dimensional regularization for soft 
 infrared divergences (mass regularization for abelian soft divergences 
 is supported as well) dimensional regularization or mass 
 regularization for collinear mass singularities complex internal 
 masses (for unstable particles) fully supported (external momenta and 
 virtualities are expected to be real) numerically dangerous regions 
 (small Gram or other kinematical determinants) cured by dedicated 
 expansions two independent implementations of all basic building 
 blocks allow for internal cross-checks cache system to speed up 
 calculations 
%prep 
%setup -q -n  COLLIER-%{version}
%if 0%{?fedora} > 35 || %{?rhel}%{!?rhel:0} >8 
export LDFLAGS=' '
%cmake -DCMAKE_INSTALL_PREFIX=%{_prefix} -DCMAKE_SKIP_INSTALL_RPATH:BOOL=ON  -DSYSCONFIG_INSTALL_DIR=%{_prefix}/share/cmake/
%else
%cmake -DCMAKE_INSTALL_PREFIX=%{_prefix} -DCMAKE_SKIP_INSTALL_RPATH:BOOL=ON  -DSYSCONFIG_INSTALL_DIR=%{_prefix}/share/cmake/  .
%endif
%build

%if 0%{?rhel} || 0%{?fedora}
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >8 
%cmake_build
#make -C x86_64-redhat-linux-gnu
%else
make
%endif
%endif

%if 0%{?suse_version}
make -C build
%endif



%install
%if 0%{?rhel} || 0%{?fedora}
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >8 
%cmake_install
#make -C x86_64-redhat-linux-gnu install DESTDIR=%{buildroot}
%else
make install DESTDIR=%{buildroot}
%endif
%endif
%if 0%{?suse_version}
make -C build install DESTDIR=%{buildroot}
%endif


%files
%defattr(-,root,root)
/usr/include/*
/usr/%_lib/*
%{_prefix}/share/cmake/*


%post 
ldconfig 

%changelog
* Fri Mar 10 2023 Andrii Verbytskyi 1.2.7
- Update to 1.2.7
* Thu Nov 23 2017 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Initial
