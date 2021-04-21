Summary:  MCFM - A parton-level Monte Carlo program MCFM.
Name: MCFM
Version: 9.1
Release: 2%{?dist}
License: GPLv3
Prefix: %{_prefix}
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  root root-core tex(latex) lhapdf-devel lhapdf cmake make
Requires:  root root-core  lhapdf-devel lhapdf libgfortran
BuildRequires: gcc-gfortran gcc-c++
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-c++ 
BuildRequires:  gcc-fortran
BuildRequires:  root6 root6-libs tex(latex) LHAPDF-devel libLHAPDF cmake make
Requires:       root6 root6-libs  libLHAPDF libgfortran5
%endif


Source: https://mcfm.fnal.gov/downloads/MCFM-%{version}.tar.gz
Prefix: %{_prefix}

%description
Parton-level Monte Carlo program MCFM. The program is designed to calculate 
cross-sections for various femtobarn-level processes at hadron-hadron 
colliders. For most processes, matrix elements are included at 
next-to-leading order and incorporate full spin correlations. For 
more details, including a list of available processes, view the documentation (PDF). 

%prep 
%setup -q 
%build 

%if %{?fedora}%{!?fedora:0} >= 31
sed -i '/^PRODUCTION/ s/$/  \-fallow\-argument\-mismatch \-fallow\-invalid\-boz \-fcommon/'  makefile
%endif



sed -i 's|\-fopenmp|\-fopenmp\ \-fPIC|g' ./TensorReduction/ov/makefile
sed -i 's|\-fopenmp|\-fopenmp\ \-fPIC|g' ./TensorReduction/makefile
sed -i 's|\-fopenmp|\-fopenmp\ \-fPIC|g' ./TensorReduction/pvext/makefile
sed -i 's|\-fopenmp|\-fopenmp\ \-fPIC|g' ./TensorReduction/recur/smallG/makefile
sed -i 's|\-fopenmp|\-fopenmp\ \-fPIC|g' ./TensorReduction/recur/makefile
sed -i 's|\-fopenmp|\-fopenmp\ \-fPIC|g' ./TensorReduction/recur/smallY/makefile
sed -i 's|\-fopenmp|\-fopenmp\ \-fPIC|g' ./TensorReduction/recur/smallF/makefile
sed -i 's|\-fopenmp|\-fopenmp\ \-fPIC|g' ./TensorReduction/recur/smallP/makefile
sed -i 's|\-fopenmp|\-fopenmp\ \-fPIC|g' ./TensorReduction/pv/makefile
sed -i 's|\-fopenmp|\-fopenmp\ \-fPIC|g' ./makefile

sed -i 's|\-O2|\-O2\ \-fPIC|g' Install
export FLAGS="-fopenmp -fPIC -O2 -pipe -Wall "
export CXXFLAGS="-fopenmp -fPIC -O2 -pipe -Wall "
export FFFLAGS="-fopenmp -fPIC -O2 -pipe -Wall "
export FCFLAGS="-fopenmp -fPIC -O2 -pipe -Wall "

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
sh Install
make
ar cr libMCFM.a obj/*.o
%endif

%if 0%{?suse_version}
sed -i '/^PRODUCTION/ s/$/  \-fallow\-argument\-mismatch \-fallow\-invalid\-boz \-fcommon/'  makefile
sh Install
make
ar cr libMCFM.a obj/*.o
%endif


%install 
mkdir -p %{buildroot}/%{_bindir}
mkdir -p %{buildroot}/%{_datadir}/%{name}
mkdir -p %{buildroot}/%{_libdir}/


mv Bin/*      %{buildroot}/%{_bindir}
mv libMCFM.a %{buildroot}/%{_libdir}/

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
