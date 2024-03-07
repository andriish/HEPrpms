Name:        rapgap
Version:    3.4.0
Release:    3%{?dist}
Summary:    Multipurpose Monte Carlo Event Generator for High Energy Physics

License:    Unknown
URL:        https://rapgap.hepforge.org/
Source0:     https://gitlab.cern.ch/jung/rapgap/-/archive/v%{version}/rapgap-v%{version}.tar.gz
#Patch0:     patch-rapgap-0.txt
BuildRequires:  autoconf automake libtool gcc-c++ tex(latex) ghostscript
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran HepMC HepMC-devel lhapdf lhapdf-devel  HepMC3  HepMC3-search HepMC3-devel HepMC3-search-devel Rivet-devel Rivet YODA YODA-devel
Requires:       libgfortran HepMC lhapdf  HepMC3 Rivet YODA HepMC3  HepMC3-search HepMC3-devel HepMC3-search-devel
BuildRequires:  fastjet-devel fastjet zlib zlib-devel 
Requires:       fastjet zlib ariadne
BuildRequires:  pythia8 pythia8-devel
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-fortran  libHepMC4 libHepMC3-3   HepMC3-devel HepMC2-devel libLHAPDF LHAPDF-devel  Rivet-devel Rivet YODA YODA-devel
Requires:       gcc-fortran libHepMC4 libHepMC3-3 libLHAPDF   Rivet YODA
BuildRequires:  fastjet-devel fastjet pkgconfig(zlib) zlib-devel unzip 
Requires:       fastjet pkgconfig(zlib)  
BuildRequires:   libpythia8 pythia-devel texlive-palatino texlive-helvetic texlive-courier tex(latex) ghostscript tex(sectsty.sty) tex(cite.sty)
%endif

%if %{?rhel}%{!?rhel:0} == 8 || %{?fedora}%{!?fedora:0} >= 31
BuildRequires: ghostscript-tools-dvipdf
%endif


%description
The Monte Carlo program RAPGAP generates a full hadron event record 
according to the HEP common standards. In ep it can describe all
 inclusive and diffractive processes, in pp it is available for 
 single-diffractive and a few inclusive processes for heavy quark and jet production. 


%prep
%setup -q -n rapgap-v%{version}

%build
sed -i 's/-std=c++1y//g' configure.ac
autoreconf -fisv

%if %{?rhel}%{!?rhel:0} || %{?fedora}%{!?fedora:0} >= 31 || 0%{?suse_version}
export CXXFLAGS='%{optflags} -std=c++1z'
%endif

%if %{?rhel}%{!?rhel:0}
export FFLAGS='%{optflags} -std=legacy -ffixed-line-length-132'
export FCFLAGS='%{optflags} -std=legacy -ffixed-line-length-132'
export LDFLAGS=' '
%endif

%if %{?fedora}%{!?fedora:0} >= 31
export FFLAGS='-std=legacy -std=legacy -fallow-argument-mismatch -fallow-invalid-boz -g'
export FCFLAGS='-std=legacy -std=legacy -fallow-argument-mismatch -fallow-invalid-boz -g'
LDFLAGS=' '
%endif

%if 0%{?suse_version}
export FFLAGS='%{optflags} -std=legacy -ffixed-line-length-132'
export FCFLAGS='%{optflags} -std=legacy -ffixed-line-length-132'
export FFLAGS="$FFLAGS  -fallow-argument-mismatch -fallow-invalid-boz -fno-strict-aliasing"
export FCFLAGS="$FCFLAGS  -fallow-argument-mismatch -fallow-invalid-boz -fno-strict-aliasing"
%endif

%configure --with-lhapdf6=/usr --with-rivet=/usr --with-hepmc3=/usr --with-hepmc2=no 

%install
make %{?_smp_mflags} 
%make_install 
rm -f $RPM_BUILD_ROOT/usr/%_lib/*la

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig


%files
/usr/%_lib/*.so*
/usr/%_lib/*.a
/usr/bin/*
/usr/include/rapgap
/usr/share/rapgap

%changelog
* Tue Jan 25 2022 Andrii verbytskyi <andrii.verbytskyi@mpp.mpg.de> - 3.310
- Bump to 3.310
* Mon Jan 17 2022 Andrii verbytskyi <andrii.verbytskyi@mpp.mpg.de> - 3.308
- Bump to 3.308
* Mon Oct 15 2018 Andrii verbytskyi <andrii.verbytskyi@mpp.mpg.de> - init
- init

