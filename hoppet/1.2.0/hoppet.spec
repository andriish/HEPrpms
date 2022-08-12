%undefine _debugsource_packages
#Scripts fail to find debug infos as the library is static

Name:          hoppet
Version:       1.2.0
Release:       6%{?dist}
License:       GPL
Prefix:        %{_prefix}
Summary:       Higher Order Perturbative Parton Evolution Toolkit
Source:        https://hoppet.hepforge.org/downloads/hoppet-%{version}.tgz
URL:           https://hoppet.hepforge.org/
%if 0%{?rhel} || 0%{?fedora}
BuildRequires: gcc-gfortran perl
Requires:      libgfortran
%endif
%if 0%{?suse_version}
BuildRequires: gcc-fortran perl
Requires:      libgfortran5
%endif

%description
HOPPET is a Fortran 95 package for carrying out QCD DGLAP evolution 
and other common manipulations of parton distribution functions (PDFs). 
It has been developed by Gavin Salam on an occasional basis since 2001 
with contributions from Juan Rojo, Frederic Dreyer and Alexander Karlberg. 

%prep 
%setup -q 

%build
%if 0%{?rhel} || 0%{?fedora}
./configure --prefix=%{buildroot}/%{_prefix}  "FFLAGS=%{optflags} -fPIC" "LDFLAGS=%{build_ldflags}"
%endif
%if 0%{?suse_version}
./configure --prefix=%{buildroot}/%{_prefix}  "FFLAGS=%{optflags} -fPIC" 
%endif

make %{?_smp_mflags}

%install 
%make_install

sed -i "s|${RPM_BUILD_ROOT}||g" $RPM_BUILD_ROOT/%{_prefix}/bin/*
sed -i "s|/lib|"/%_lib"|g" $RPM_BUILD_ROOT/%{_prefix}/bin/hoppet-config
mv %{buildroot}/%{_prefix}/lib  %{buildroot}/%_libdir

%files
%defattr(-,root,root)
%_bindir/hoppet-config
%_libdir/*
%_includedir/*

%clean
rm -rf %{buildroot}

%post 
ldconfig 

%changelog
* Sun Feb 21 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Cleanup            
* Thu Nov 23 2017 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Initial
