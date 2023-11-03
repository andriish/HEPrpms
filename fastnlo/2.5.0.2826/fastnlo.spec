Name: fastnlo
Version: 2.5.0.2826
Release: 4%{?dist}
License: GPL
Prefix: %{_prefix}
Summary: Fast pQCD calculations for PDF fits.
Source:  https://fastnlo.hepforge.org/code/v25/fastnlo_toolkit-2.5.0-2826.tar.gz
URL:     https://fastnlo.hepforge.org/
#A patch is needed to make this work with python

%if 0%{?rhel} || 0%{?fedora}
BuildRequires: gcc-gfortran gcc-c++ lhapdf-devel lhapdf   root root-core 
BuildRequires: qcdnum qcdnum-devel hoppet YODA YODA-devel fastjet fastjet-devel  zlib zlib-devel
BuildRequires: autoconf automake libtool tex(latex) swig doxygen texlive-epstopdf ghostscript
Requires:      YODA qcdnum fastjet 
%endif
%if 0%{?suse_version}
BuildRequires: gcc-fortran gcc-c++ LHAPDF-devel libLHAPDF   root6 root6-libs root6-devel 
BuildRequires: qcdnum qcdnum-devel hoppet YODA YODA-devel fastjet fastjet-devel  pkgconfig(zlib) zlib-devel
BuildRequires: autoconf automake libtool tex(latex) swig doxygen texlive-epstopdf ghostscript
Requires:      YODA qcdnum fastjet 
%endif


%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
BuildRequires: python3 python3-devel
%endif
%if 0%{?suse_version}
BuildRequires: python python-devel
%endif


%package  devel
Summary:        Libraries and headers for %{name}
Provides:       %{name}-devel = %{version}-%{release}

%description  devel
Contains the libraries and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.


%if %{?rhel}%{!?rhel:0} >= 8
%package  -n python-%{name}
Summary:        python bindings for %{name}
Provides:       python3-%{name} = %{version}-%{release}

%description -n python-%{name}
python-%{name} contains python bindings for %{name}.
%endif

%description
The fastNLO project provides computer code to create and evaluate fast 
interpolation tables of pre-computed coefficients in perturbation theory 
for observables in hadron-induced processes.

%prep 
%setup -q -n fastnlo_toolkit-2.5.0-2826

%build 


sed -i 's|\#\$(DEPDIR)/fastnlo_wrap.Plo:|\$(DEPDIR)/fastnlo_wrap.Plo:|g' pyext/Makefile.in
%if 0%{?rhel} 
%configure --disable-doxygen-doc  --with-lhapdf=/usr --with-hoppet --with-root --with-yoda --with-fastjet --with-qcdnum --enable-pyext3 
%endif

%if 0%{?suse_version} || 0%{?fedora}
%configure --disable-doxygen-doc  --with-lhapdf=/usr --with-hoppet --with-root --with-yoda --with-fastjet --with-qcdnum  
%endif

make %{?_smp_mflags}

%install 
%make_install
%if 0%{?rhel} 
mkdir -p %{buildroot}/%{python3_sitearch}/
mv %{buildroot}/usr/lib/python*/site-packages/*  %{buildroot}/%{python3_sitearch}/
%endif

%files
%defattr(-,root,root)
/usr/bin/fnlo-tk*
/usr/%_lib/libfastnlotoolkit*
/usr/share/fastnlo_toolkit/modify/SteerModify.str

%files devel
/usr/include/*


%if %{?rhel}%{!?rhel:0} >= 8
%files -n python-%{name}
%{python3_sitearch}/*
%endif

%post 
ldconfig 

%changelog
* Fri Aug 12 2022 Andrii Verbytskyi 2.5.0
- Use Python3
* Mon Nov 15 2021 Andrii Verbytskyi 2.5.0
- Bump to 2.5.0
* Sun Feb 21 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Cleanup           
* Thu Nov 23 2017 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Initial
