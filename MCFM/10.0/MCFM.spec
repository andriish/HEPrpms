Summary:  MCFM - A parton-level Monte Carlo program MCFM.
Name: MCFM
Version: 10.0
Release: 1%{?dist}
License: GPLv3
Prefix: %{_prefix}
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
%setup -q -n CuTe-MCFM 
%patch0 -p1

sed -i   '/^[^#]*\.f/  s/^[ \t]*//' src/*/*txt
sed -i   '/^[^#]*\.cpp/  s/^[ \t]*//' src/*/*txt
sed -i   '/^[^#]*\.f/ s/^/\$\{CMAKE_CURRENT_SOURCE_DIR}\//'  src/*/*txt
sed -i   '/^[^#]*\.cpp/ s/^/\$\{CMAKE_CURRENT_SOURCE_DIR}\//'  src/*/*txt

sed -i   '/^[^#]*\.f/  s/^[ \t]*//' TensorReduction/*/*txt
sed -i   '/^[^#]*\.cpp/  s/^[ \t]*//' TensorReduction/*/*txt
sed -i   '/^[^#]*\.f/ s/^/\$\{CMAKE_CURRENT_SOURCE_DIR}\//'  TensorReduction/*/*txt
sed -i   '/^[^#]*\.cpp/ s/^/\$\{CMAKE_CURRENT_SOURCE_DIR}\//'  TensorReduction/*/*txt

sed -i   '/^[^#]*\.f/  s/^[ \t]*//' TensorReduction/*/*/*txt
sed -i   '/^[^#]*\.cpp/  s/^[ \t]*//' TensorReduction/*/*/*txt
sed -i   '/^[^#]*\.f/ s/^/\$\{CMAKE_CURRENT_SOURCE_DIR}\//'  TensorReduction/*/*/*txt
sed -i   '/^[^#]*\.cpp/ s/^/\$\{CMAKE_CURRENT_SOURCE_DIR}\//'  TensorReduction/*/*/*txt


%build 
export FFLAGS="%{optflags} -I$(lhapdf-config --incdir) -fPIC -fno-var-tracking-assignments  "
export FCFLAGS="%{optflags}  -fPIC -fno-var-tracking-assignments "
export CFLAGS="%{optflags}  -fPIC"
export CXXFLAGS="%{optflags}  -fPIC"

%if 0%{?fedora}
export FFLAGS=" -g -I$(lhapdf-config --incdir) -fPIC -fno-var-tracking-assignments  "
export FCFLAGS=" -g -fPIC -fno-var-tracking-assignments "
export LDFLAGS=" "
%endif

%cmake -Duse_external_lhapdf:BOOL=ON -Duse_internal_lhapdf:BOOL=OFF
%cmake_build


%install 
%cmake_install

%files
%defattr(-,root,root)
%{_bindir}/*
%{_libdir}/libMCFM.a

%clean
rm -rf %{buildroot}

%post 
ldconfig 

%changelog             
* Thu Nov 28 2019 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 8.1
- Initial
