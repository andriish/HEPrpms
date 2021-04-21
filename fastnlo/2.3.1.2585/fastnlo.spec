Name: fastnlo
Version: 2.3.1.2585
Release: 2%{?dist}
License: GPL
Prefix: %{_prefix}
Summary: Fast pQCD calculations for PDF fits
Source:  https://fastnlo.hepforge.org/code/v23/fastnlo_toolkit-2.3.1-2585.tar.gz
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
BuildRequires: python2 python2-devel
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


%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%package  -n python2-%{name}
Summary:        python bindings for %{name}
Provides:       python2-%{name} = %{version}-%{release}

%description -n python2-%{name}
python2-%{name} contains python bindings for %{name}.
%endif
%if 0%{?suse_version}
%package  -n python-%{name}
Summary:        python bindings for %{name}
Provides:       python3-%{name} = %{version}-%{release}

%description -n python-%{name}
python2-%{name} contains python bindings for %{name}.
%endif



%description
The fastNLO project provides computer code to create and evaluate fast 
interpolation tables of pre-computed coefficients in perturbation theory 
for observables in hadron-induced processes.

%prep 
%setup -q -n fastnlo_toolkit-2.3.1-2585

%build 

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
export PYTHON=%{_bindir}/python2
export PYTHON_VERSION=PYTHON_VERSION=2.7
%endif
%if 0%{?suse_version}
export PYTHON=%{_bindir}/python
export PYTHON_VERSION=2.7
%endif
sed -i 's|\#\$(DEPDIR)/fastnlo_wrap.Plo:|\$(DEPDIR)/fastnlo_wrap.Plo:|g' pyext/Makefile.in
%configure --disable-doxygen-doc  --with-lhapdf=/usr --with-hoppet --with-root --with-yoda --with-fastjet --with-qcdnum --enable-pyext 
make %{?_smp_mflags}

%install 
%make_install
%if 0%{?rhel} || 0%{?fedora}
mkdir -p %{buildroot}/%{python2_sitearch}/
mv %{buildroot}/usr/lib/python*/site-packages/*  %{buildroot}/%{python2_sitearch}/
%endif

%if 0%{?suse_version}
mv %{buildroot}/usr/lib/python2.7/site-packages/*  %{buildroot}/usr/%_lib/python2.7/site-packages/
%endif


%files
%defattr(-,root,root)
/usr/bin/fnlo-tk*
/usr/%_lib/libfastnlotoolkit*
/usr/share/fastnlo_toolkit/modify/SteerModify.str

%files devel
/usr/include/*


%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%files -n python2-%{name}
%{python2_sitearch}/*
%endif

%if 0%{?suse_version}
%files -n python-%{name}
/usr/%_lib/python2.7/site-packages/*
%endif



%post 
ldconfig 

%changelog
* Sun Feb 21 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Cleanup           
* Thu Nov 23 2017 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - Initial
