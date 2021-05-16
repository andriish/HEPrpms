Name:        rapgap
Version:    3.303
Release:    9%{?dist}
Summary:    Multipurpose Monte Carlo Event Generator for High Energy Physics

License:    Unknown
URL:        https://rapgap.hepforge.org/
Source0:    https://rapgap.hepforge.org/downloads/rapgap-%{version}.tar.gz
Patch0:     patch-rapgap-0.txt
BuildRequires:  autoconf automake libtool gcc-c++ tex(latex) ghostscript
%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  gcc-gfortran HepMC HepMC-devel lhapdf lhapdf-devel pythia6 HepMC3  HepMC3-search HepMC3-devel HepMC3-search-devel Rivet-devel Rivet YODA YODA-devel
Requires:       libgfortran HepMC lhapdf  pythia6 HepMC3 Rivet YODA HepMC3  HepMC3-search HepMC3-devel HepMC3-search-devel
BuildRequires:  fastjet-devel fastjet zlib zlib-devel ariadne
Requires:       fastjet zlib ariadne
BuildRequires:  pythia8 pythia8-devel
%endif
%if 0%{?suse_version}
BuildRequires:  gcc-fortran  libHepMC4 libHepMC3-1   HepMC3-devel HepMC2-devel libLHAPDF LHAPDF-devel pythia6 Rivet-devel Rivet YODA YODA-devel
Requires:       gcc-fortran libHepMC4 libHepMC3-1 libLHAPDF  pythia6 Rivet YODA
BuildRequires:  fastjet-devel fastjet pkgconfig(zlib) zlib-devel unzip ariadne
Requires:       fastjet pkgconfig(zlib)  ariadne
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
%setup -q
%patch0 -p1

%build
autoreconf -fisv
#sed -i 's/[^[:print:]\r\t]//g' src/*/*f
#sed -i 's/[^[:print:]\r\t]//g' src/*/*F
#sed -i 's/[^[:print:]\r\t]//g' bases51/*f   
#sed -i 's/[^[:print:]\r\t]//g' misc/*F   

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

export ARIADNE=/usr
%configure --with-lhapdf=/usr --with-hepmc=/usr  --with-pythia6=/usr --with-rivet=/usr --with-hepmc3=/usr ariadne=yes

%install
make %{?_smp_mflags} 
%make_install 

mkdir $RPM_BUILD_ROOT/usr/share/rapgap
mv $RPM_BUILD_ROOT/usr/share/*.*  $RPM_BUILD_ROOT/usr/share/rapgap
mv $RPM_BUILD_ROOT/usr/share/steer*  $RPM_BUILD_ROOT/usr/share/rapgap

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig


%files
/usr/%_lib/*.so*
/usr/%_lib/*a
/usr/bin/*
/usr/include/rapgap
/usr/share/rapgap

%changelog
* Mon Oct 15 2018 Andrii verbytskyi <andrii.verbytskyi@mpp.mpg.de> - init
- init

