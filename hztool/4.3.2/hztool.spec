%define major       0
%define libname     hztool
%define libnamedev  hztool-devel
%define develnamestatic  hztool-static-devel
#undefine _debugsource_packages
#Scripts fail to find debug infos

Name:           hztool
Version:        4.3.2
Release:        2%{?dist}
License:        GPLv2
Url:            http://www.hztool.fr
Source0:        https://hztool.hepforge.org/downloads/%{name}-4.3.2.tar.gz
Patch0:         patch-hztool-0.txt

Prefix: %{_prefix}
Summary:       Library of routines for Monte Carlo to data comparison
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran gcc-c++
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-fortran gcc-c++
%endif
BuildRequires:  tex(latex) ghostscript 
BuildRequires:  autoconf automake libtool 
%if %{?fedora}%{!?fedora:0}  
BuildRequires: tex(moreverb.sty)
%endif

%description
HZTool is a library of routines which will allow you to reproduce an 
experimental result using the four-vector final state from Monte Carlo generators.

%prep
%setup -q
%patch0 -p1

%build
autoreconf --force --install --verbose .

export FLAGS='%{optflags} -std=legacy -ffixed-line-length-132'
export FFLAGS='%{optflags} -std=legacy -ffixed-line-length-132'
export FCFLAGS='%{optflags} -std=legacy -ffixed-line-length-132'
#specific 
export MYFFLAGS='%{optflags} -std=legacy -ffixed-line-length-132'
export LDFLAGS=' '

%if %{?fedora}%{!?fedora:0} >= 31
export FLAGS="$FLAGS  -fallow-argument-mismatch -fallow-invalid-boz"
export FFLAGS="$FFLAGS  -fallow-argument-mismatch -fallow-invalid-boz"
export FCFLAGS="$FCFLAGS  -fallow-argument-mismatch -fallow-invalid-boz"
export MYFFLAGS="$MYFFLAGS -fallow-argument-mismatch -fallow-invalid-boz -fno-automatic -fno-backslash -fbounds-check -fpic  -Wl,-flat_namespace"
%endif
%if 0%{?suse_version}
export FLAGS="$FLAGS  -fallow-argument-mismatch -fallow-invalid-boz"
export FFLAGS="$FFLAGS  -fallow-argument-mismatch -fallow-invalid-boz"
export FCFLAGS="$FCFLAGS  -fallow-argument-mismatch -fallow-invalid-boz"
export MYFFLAGS="$MYFFLAGS -fallow-argument-mismatch -fallow-invalid-boz -fno-automatic -fno-backslash -fbounds-check -fpic  -Wl,-flat_namespace"
%endif

%if %{?rhel}%{!?rhel:0} 
%configure --disable-docs
%endif

%if %{?fedora}%{!?fedora:0} 
%configure 
%endif


%if 0%{?suse_version}
%configure --disable-docs
%endif

%install


%make_install
find $RPM_BUILD_ROOT -type f -name '*.la' -exec rm -f {} \;



%files -n %{libname}

%{_includedir}/%{name}/*
%{_libdir}/*
/usr/share/*

%changelog
* Sun Jan 24 2016 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 4.3.2-1
+ test
