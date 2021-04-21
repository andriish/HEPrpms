%define major       0
%define libname     golem95

Name:           golem95
Version:        1.3.3
Release:        1%{?dist}
License:        GPL
Url:            http://www.hepforge.org/archive/golem/
Source0:        http://www.hepforge.org/archive/golem/%{name}-%{version}.tar.gz
Summary:        GOLEM a package for the numerical  evaluation of integrals
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-fortran
%endif
Prefix: %{_prefix}

%description
 The program golem95 is a package for the numerical 
 evaluation of integrals and tensor form factors 
 entering the calculation of one-loop 
 amplitudes with up to six external legs, written in Fortran 95.
 golem95 performs the reduction to a certain set of basis integrals 
 numerically, using a formalism where inverse Gram determinants 
 can be avoided. The library contains massless as well as massive 
 integrals and also can handle complex masses. Infrared and 
 ultraviolet divergences are regulated by dimensional 
 regularization. Integrals with tensor ranks exceeding the number of propagators are supported.


%package -n     %{name}-devel
Summary:        Libraries for %{name}
Provides:       %{name} = %{version}-%{release}

%description -n     %{name}-devel
 The program golem95 is a package for the numerical 
 evaluation of integrals and tensor form factors 
 entering the calculation of one-loop 
 amplitudes with up to six external legs, written in Fortran 95.


%prep
%setup -q

%build
%configure  
make 

%install
%make_install

%files 
%doc AUTHORS README COPYING
%{_libdir}/*

%files -n %{name}-devel
%{_includedir}/*

%changelog
* Mon Feb 3 2014 Andrii Verbytskyi 1.3.3
+ Initial spec file

