%define debug_package %{nil}
Summary:  A parton level Monte Carlo for processes with electroweak bosons
Name: VBFNLO
Version: 3.0.0beta5
Release: 10%{?dist}
License: GPLv2
Source: https://github.com/vbfnlo/vbfnlo/archive/v%{version}.tar.gz
URL:    https://www.itp.kit.edu/vbfnlo/wiki/doku.php
Patch1:         patch-VBFNLO-1.txt
Prefix: %{_prefix}
%if 0%{?rhel} || 0%{?fedora}
BuildRequires: HepMC-devel root-core gsl gsl-devel lhapdf-devel autoconf automake libtool gcc-gfortran  root-genvector
BuildRequires: HepMC  lhapdf libgfortran 
Requires: HepMC root-core gsl gsl-devel lhapdf libgfortran root-genvector
%endif
%if 0%{?suse_version}
BuildRequires: HepMC2-devel root6-libs root6-config root6-devel root6 gsl gsl-devel LHAPDF-devel autoconf automake libtool gcc-fortran  
BuildRequires: libHepMC4  libLHAPDF gcc-fortran 
Requires: libHepMC4 root6-config root6-libs root6-devel gsl gsl-devel libLHAPDF gcc-fortran
%endif

%description
VBFNLO is a fully flexible parton level Monte Carlo program for the simulation 
of vector boson fusion, double and triple vector boson production in hadronic 
collisions at next to leading order in the strong coupling constant. 
VBFNLO includes Higgs and vector boson decays with full spin correlations and
all off-shell effects. In addition, VBFNLO implements CP-even and CP-odd Higgs 
boson via gluon fusion, associated with two jets, at the leading order one loop 
level with the full top-quark and bottom-quark mass dependence in a generic two 
Higgs doublet model.

%prep 
%setup  -n vbfnlo-%{version}
%patch -P 1 -p 1

%build 

test -n "$srcdir" || srcdir=`dirname .`
test -n "$srcdir" || srcdir=.
autoreconf --force --install --verbose "$srcdir"
test -n "$NOCONFIGURE" || "$srcdir/configure" "$@"

%if 0%{?rhel} || 0%{?fedora}
%if %{?fedora}%{!?fedora:0} >= 31 || %{?rhel}%{!?rhel:0} > 8
export FLAGS='-std=legacy -fallow-argument-mismatch -fallow-invalid-boz -fno-var-tracking-assignments'
export FFLAGS='-std=legacy -fallow-argument-mismatch -fallow-invalid-boz -fno-var-tracking-assignments'
export FCFLAGS='-std=legacy -fallow-argument-mismatch -fallow-invalid-boz -fno-var-tracking-assignments' 
LDFLAGS=' '
%else
export FFLAGS=' -std=legacy -fno-var-tracking-assignments '
%endif
%endif
%if 0%{?suse_version}
export FLAGS='-std=legacy -fallow-argument-mismatch -fallow-invalid-boz -fno-var-tracking-assignments'
export FFLAGS='-std=legacy -fallow-argument-mismatch -fallow-invalid-boz -fno-var-tracking-assignments'
export FCFLAGS='-std=legacy -fallow-argument-mismatch -fallow-invalid-boz -fno-var-tracking-assignments' 
LDFLAGS=' '
%endif

%configure \
--enable-processes=all \
--enable-kk \
--enable-spin2 \
--with-gsl \
--enable-quad=no \
--with-LHAPDF \
--with-LOOPTOOLS \
--with-root=/usr \
--with-hepmc

make

%install 
make DESTDIR=%{buildroot} install

%files
%defattr(-,root,root)
/usr/bin/*
/usr/include/VBFNLO/*
/usr/%_lib/VBFNLO/*
/usr/share/VBFNLO/*

%clean
rm -rf %{buildroot}

%post 
ldconfig 

%changelog             
* Thu Nov 23 2017 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - 2.7.1
* Thu Nov 23 2017 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
 - 2.7.0-3
* Tue Apr 22 2014 Ben Meekhof <bmeekhof@umich.edu> - 2.7.0-1
- Update to latest release
- Set --enable-quad=no.  New option since 2.6.3 and configure fails if enabled.
* Mon Apr 7 2014 Ben Meekhof <bmeekhof@umich.edu> - 2.6.3-1
- initial packaging

