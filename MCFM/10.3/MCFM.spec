Summary:  A parton-level Monte Carlo event generator
Name: MCFM
Version: 10.3
Release: 3%{?dist}
License: GPLv3
Prefix: %{_prefix}
URL:     https://mcfm.fnal.gov/

%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  lhapdf-devel lhapdf
Requires:       lhapdf libgfortran
BuildRequires:  gcc-gfortran
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-fortran
BuildRequires:  LHAPDF-devel libLHAPDF
Requires:       libLHAPDF libgfortran5
%endif
BuildRequires: gcc-c++ make tex(latex)
BuildRequires: cmake >= 3.4.3


Source0: https://mcfm.fnal.gov/downloads/MCFM-%{version}.tar.gz
Patch0: patch-MCFM-0.txt
Prefix: %{_prefix}

%description
Parton-level Monte Carlo program MCFM. The program is designed to calculate 
cross-sections for various femtobarn-level processes at hadron-hadron 
colliders. For most processes, matrix elements are included at 
next-to-leading order and incorporate full spin correlations. For 
more details, including a list of available processes, view the documentation (PDF). 

%prep 
%setup -q -n MCFM-%{version}
%patch -P 0 -p1

%build 

%if 0%{?fedora} || %{?rhel}%{!?rhel:0} >= 8 
export FFLAGS=" -g -I$(lhapdf-config --incdir) -fPIC -fno-var-tracking-assignments  "
export FCFLAGS=" -g -fPIC -fno-var-tracking-assignments "
export CXXFLAGS="%{optflags}  -fPIC"
export LDFLAGS=" "
%else
export FFLAGS="%{optflags} -I$(lhapdf-config --incdir) -fPIC -fno-var-tracking-assignments  "
export FCFLAGS="%{optflags}  -fPIC -fno-var-tracking-assignments "
export CFLAGS="%{optflags}  -fPIC"
export CXXFLAGS="%{optflags}  -fPIC"
%endif

%if 0%{?fedora} || %{?rhel}%{!?rhel:0}
%cmake -Duse_external_lhapdf:BOOL=ON -Duse_internal_lhapdf:BOOL=OFF -Dwith_library:BOOL=ON -S. -BBUILD
cmake --build BUILD
%else
%cmake -Duse_external_lhapdf:BOOL=ON -Duse_internal_lhapdf:BOOL=OFF -Dwith_library:BOOL=ON  -BBUILD
cmake --build BUILD
%endif

%install 
%if 0%{?fedora} || %{?rhel}%{!?rhel:0}
%make_install -C BUILD
%else
%make_install -C build/BUILD
%endif
find $RPM_BUILD_ROOT -type f -name '._CXX_Interface.h' -exec rm -f {} \;
find $RPM_BUILD_ROOT -type f -name '._CXX_Wrapper.h' -exec rm -f {} \;
find $RPM_BUILD_ROOT -type f -name '._Flavor_Map.h' -exec rm -f {} \;

%files
%defattr(-,root,root)
%{_bindir}/*
%{_includedir}/MCFM/*
%{_libdir}/lib*

%clean
rm -rf %{buildroot}

%post 
ldconfig 

%changelog
* Fri Mar 10 2023 Andrii Verbytskyi 10.3
- Update to 10.3
* Fri Aug 12 2022 Andrii Verbytskyi 10.2.1
- Bump to 10.2.1   
* Mon Nov 15 2021 Andrii Verbytskyi 10.0.1
- Bump to 10.0.1           
* Thu Nov 28 2019 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 8.1
- Initial
