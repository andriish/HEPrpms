Name:           chaplin
Version:        1.2
Release:        2%{?dist}
License:        Standard CPC licence
Url:            http://chaplin.hepforge.org/
Source0:        http://chaplin.hepforge.org/code/%{name}-%{version}.tar
Summary:        CHAPLIN - Complex Harmonic Polylogarithms in FORTRAN
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran gcc-c++ hoppet 
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-fortran gcc-c++ hoppet 
%endif
Prefix:         %{_prefix}

%description
The FORTRAN library CHAPLIN enables you to numerically evaluate Harmonic
polylogarithms up to weight 4 for any complex argument. It is presented 
in arXiv:1106.5739. Features: Numerical routines to evaluate any Harmonic 
polylogarithm of weight lower or equal to four. Straightforward interface 
to C/C++ code. Installs a shared library on you system, either globally 
or in a local directory. 


%prep
%setup -q

%build
%configure
make  %{?_smp_mflags} 

%install
%make_install 

%files 
%_libdir/*


%changelog
* Sun Feb 21 2021 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 1.2
- Cleanup
* Mon Feb 3 2014 Andrii Verbytskyi 1.0.4
+ Initial spec file

