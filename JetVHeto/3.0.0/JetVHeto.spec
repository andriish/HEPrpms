Summary:  A program to perform NNLL resummation for jet-veto efficiencies and cross sections in Higgs and Z-boson production
Name: JetVHeto
Version: 3.0.0
Release: 2%{?dist}
License: GPLv3
Source: https://www.hepforge.org/downloads/JetVHeto/JetVHeto-%{version}.tgz
URL:    https://jetvheto.hepforge.org/

%if 0%{?rhel} || 0%{?fedora}
Requires:      hoppet lhapdf chaplin
BuildRequires: hoppet lhapdf-devel lhapdf chaplin
BuildRequires: gcc-gfortran gcc-c++
%endif
%if 0%{?suse_version}
Requires:      hoppet libLHAPDF chaplin
BuildRequires: hoppet LHAPDF-devel libLHAPDF chaplin
BuildRequires: gcc-fortran gcc-c++
%endif

Prefix: %{_prefix}

%description
a user-friendly, fast program to perform NNLL resummation for jet-veto 
efficiencies and cross sections in Higgs and Z-boson production, as well 
as resummation in the leading logarithms of the jet radius. It also
 performs NNLL resummation for the Higgs and Z-boson transverse momentum. 
%prep 
%setup -q 
%build 

sed -i 's@CHAPLIN=@FOOCHAPLIN=@' Makefile
make CHAPLIN=%{_libdir} FFLAGS="%{optflags}"

%install 
mkdir -p $RPM_BUILD_ROOT/%{_bindir}
cp -a jetvheto $RPM_BUILD_ROOT/%{_bindir}

%files
%defattr(-,root,root)
%{_bindir}/jetvheto


%post 
ldconfig 

%changelog             
* Fri Nov 29 2019 Andrii Verbytskyi 3.0.0
 - Initial
