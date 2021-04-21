Name:       cascade
Version:    3.0.01
Release:    7%{?dist}
Summary:    Cascade Event Generator for High Energy Physics

License:    GPLv2
URL:        https://cascade.hepforge.org/
Source0:    https://github.com/andriish/cascade/archive/master.zip 
#cascade-3.0.01-beta01.tar.gz

%if 0%{?rhel} || 0%{?fedora}
BuildRequires:  autoconf automake libtool gcc-gfortran HepMC HepMC-devel lhapdf lhapdf-devel 
BuildRequires:  pythia6 HepMC3  HepMC3-search HepMC3-devel HepMC3-search-devel Rivet-devel Rivet YODA YODA-devel
BuildRequires:  tex(latex) ghostscript tex(sectsty.sty)
Requires:       libgfortran HepMC lhapdf  pythia6 HepMC3 Rivet YODA HepMC3  HepMC3-search HepMC3-devel HepMC3-search-devel
BuildRequires:  gsl gsl-devel boost boost-devel
Requires:       gsl boost zlib
BuildRequires:  fastjet-devel fastjet gcc-c++ gcc-gfortran zlib-devel tmdlib tmdlib-devel pythia8 pythia8-devel
Requires:       fastjet tmdlib
%endif
%if 0%{?suse_version}
BuildRequires:  autoconf automake libtool gcc-fortran libHepMC4 HepMC2-devel libLHAPDF LHAPDF-devel 
BuildRequires:  pythia6 libHepMC3-1  HepMC3-devel  Rivet-devel Rivet YODA YODA-devel 
BuildRequires:  texlive-palatino texlive-helvetic texlive-courier tex(latex) ghostscript tex(sectsty.sty) tex(cite.sty)
Requires:       libHepMC4 libLHAPDF  pythia6 libHepMC3-1 Rivet YODA libHepMC4 
BuildRequires:  gsl gsl-devel  boost-devel unzip
Requires:       gsl  zlib
BuildRequires:  fastjet-devel fastjet gcc-c++ gcc-fortran zlib-devel tmdlib tmdlib-devel  libpythia8 pythia-devel
Requires:       fastjet tmdlib libpythia8
%endif

Prefix: %{_prefix}
%if %{?rhel}%{!?rhel:0} == 8
BuildRequires: ghostscript-tools-dvipdf
%endif
%description
The Monte Carlo program CASCADE generates a full hadron event record 
according to the HEP common standards. CASCADE was originally intended 
for small x processes and used only gluon chains in the initial state cascade.
With the development of the Parton Branching TMDs, which are valid over 
a large range in x and Q2, a new development has started (CASCADE3, 
CASCADE-lhe): CASCADE-lhe makes use of LHE files (either from collinear 
NLO calcualtions like POWHEG or MC@NLO or off-shell calculations from KaTie) 
and has a full flavor initial state parton shower, which follows directly 
the TMD from the Parton Brachning method. Different sets of TMDs are 
now accessed via TMDlib. 

%prep
%setup -q -n  cascade-master/cascade-%{version}-beta01

%build
autoreconf -fisv


%configure --with-hepmc=/usr --with-tmdlib=/usr --with-lhapdf=/usr --with-pythia8=/usr --with-gsl=/usr --with-boost=/usr/include  --with-pythia6=/usr


%install
make  

%make_install 
find %{buildroot} -name '*.la' -delete

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig


%files
%{_bindir}/*
%{_libdir}/*.so*
%{_libdir}/*.a
%{_includedir}/*
/usr/share/*




%changelog
* Fri Nov 29 2019 Andrii Verbytskyi 3.0.01
+ Cleanup
* Mon Oct 15 2018 Andrii verbytskyi <andrii.verbytskyi@mpp.mpg.de> - 3.0.01
- init
