%define major            0
%define libname          TAUOLA
%define libnamedev       TAUOLA-devel
%define develnamestatic  TAUOLA-static-devel
%undefine _debugsource_packages


Name:           TAUOLA
Version:        1.1.6c
Release:        2%{?dist}
License:        Unknown
Url:            http://tauolapp.web.cern.ch/tauolapp
Source0:        http://tauolapp.web.cern.ch/tauolapp/resources/%{name}.%{version}/%{name}.%{version}.tar.gz
Patch0:         patch-TAUOLA-0.txt
Patch1:         patch-TAUOLA-1.txt
Summary:        Tau lepton decay Monte Carlo
BuildRequires:  gcc-gfortran gcc-c++ HepMC-devel autoconf automake libtool 
Requires:       HepMC libgfortran
Prefix: %{_prefix}
%description
Because of their narrow width, tau decays can be well separated from 
their production process. Only spin degrees of freedom connect these 
two parts of the physics process of interest for high energy collision 
experiments. In the following, we present a Monte Carlo algorithm which 
is based on that property. The interface supplements events generated 
by other programs, with tau decays




%package  devel
Summary:        Libraries and headers for %{name}
Requires:       %{libname} = %{version}
Provides:       %{name}-devel = %{version}-%{release}

%description devel
%{libnamedev} contains the libraries and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.



%prep
%setup -qn %{name}
%patch0 -p0
%patch1 -p0

%build
sed -i 's/AC_FUNC_MALLOC/#NOAC_FUNC_MALLOC/g' configure.in
sed -i 's/AC_FUNC_STRFTIME/#NOAC_FUNC_STRFTIME/g' configure.in
autoreconf
%configure  --with-hepmc=%prefix
make 


%install
%make_install
%files 
/usr/%_lib/*
%files -n %{libnamedev}
%{_includedir}/Tauola/*




%changelog
* Fri Dec 22 2017 Andrii Verbytskyi 1.1.6c
+ New version
* Thu May 26 2016 Andrii Verbytskyi 1.1.5
+ Initial spec file

