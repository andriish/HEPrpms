%define major       0
%define libname     SHERPA-MC
%define sharename   SHERPA-MC-common
%define openmpiname SHERPA-MC-openmpi
%define libnamedev  SHERPA-MC-devel
%define develnamestatic  SHERPA-MC-static-devel
%define _unpackaged_files_terminate_build 0
# Define a macro for calling ../configure instead of ./configure
%global dconfigure %(printf %%s '%configure' | sed 's!\./configure!../configure!g')
%global _missing_build_ids_terminate_build 0
# --enable-analysis  is not set


Name:           SHERPA-MC
Version:        2.2.12
Release:        7%{?dist}
License:        GPLv2
Url:              https://sherpa.hepforge.org
Source0:          https://sherpa.hepforge.org/downloads/%{name}-%{version}.tar.gz
Summary:          Multipurpose Monte Carlo Event Generator for High Energy physics
Patch0:           patch-SHERPA-MC-0.txt

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} 
BuildRequires:    gcc-gfortran gcc-c++ root pythia8-devel pythia8   Rivet Rivet-devel hztool
BuildRequires:    root-core root HepMC3-rootIO-devel HepMC3-devel HepMC3-search HepMC3-search-devel  HepMC3-rootIO HepMC3  HepMC HepMC-devel 
BuildRequires:    autoconf automake libtool sqlite-devel sqlite subversion root-physics libzip-devel openmpi-devel
BuildRequires:    openmpi environment-modules lhapdf-devel fastjet fastjet-devel  YODA-devel Rivet-devel zlib zlib-devel
Requires:         HepMC lhapdf  HepMC3-rootIO HepMC3  sqlite root root-core openloops root Rivet YODA pythia8 openmpi fastjet  zlib
Requires:         hztool libgfortran  recola qd openssl  HepMC3-search
BuildRequires:    swig  recola qd qd-devel openssl-devel openssl
Requires:         blackhat blackhat-data
BuildRequires:    blackhat-devel blackhat
BuildRequires:    texinfo git
%endif
%if 0%{?suse_version}
BuildRequires:    gcc-fortran gcc-c++ pythia-devel libpythia8   Rivet Rivet-devel hztool
BuildRequires:    root6-libs root6-devel root6-config root6 HepMC3-devel  libHepMC4 HepMC2-devel 
BuildRequires:    autoconf automake libtool sqlite-devel sqlite subversion  libzip-devel openmpi3-devel
BuildRequires:    openmpi environment-modules LHAPDF-devel fastjet fastjet-devel  YODA-devel Rivet-devel zlib zlib-devel
Requires:         libHepMC4 libLHAPDF libHepMC4  sqlite root6 root6-libs openloops Rivet YODA libpythia8 openmpi3 fastjet  zlib
Requires:         hztool gcc-fortran  recola libqd0 openssl
BuildRequires:    swig  recola libqd0 qd-devel openssl-devel openssl
Requires:         blackhat blackhat-data libgfortran5
BuildRequires:    blackhat-devel blackhat
BuildRequires:    texinfo git
%endif


%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} == 8
BuildRequires: python3-distutils-extra
%endif
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
BuildRequires:    python3 python3-devel 
Requires:         python3
Requires:         cernlib cernlib-devel cernlib-static
BuildRequires:    cernlib cernlib-devel cernlib-static
Requires:  MCFM
BuildRequires: MCFM
%endif

%if 0%{?suse_version}
BuildRequires:    python3 python3-devel
Requires:         python3
Requires:         cernlib cernlib-devel cernlib-static
BuildRequires:    cernlib cernlib-devel cernlib-static
Requires:  MCFM
BuildRequires: MCFM
%endif


Prefix: %{_prefix}

%description
Sherpa is a Monte Carlo event generator for the Simulation of High-Energy 
Reactions of PArticles in lepton-lepton, lepton-photon, photon-photon, 
lepton-hadron and hadron-hadron collisions. Simulation programs - also 
dubbed event generators - like Sherpa are indispensable work horses for 
current particle physics phenomenology and are (at) the interface between 
theory and experiment.  


%package  devel
Summary:        Libraries and headers for %{name}
Provides:       %{name}-devel = %{version}-%{release}

%description devel
%{libnamedev} contains the libraries and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.

%package  common
Summary:        Libraries and headers for %{name}

Provides:       %{name}-common = %{version}-%{release}

%description common
Contains the common files and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.




%package  openmpi
Summary:        Libraries and headers for %{name}

Provides:       %{name}-openmpi = %{version}-%{release}

%description openmpi
Contains the common files and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.

%package  openmpi-devel
Summary:        Libraries and headers for %{name}

Provides:       %{name}-openmpi-devel = %{version}-%{release}

%description openmpi-devel
%{libnamedev} contains the libraries and header files needed to
develop programs which make use of %{name}.
The library documentation is available on header files.


%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0}
%package -n python3-%{name}
Summary:   %{name}  Python 3 bindings
%{?python_provide:%python_provide python3-%{name}}
%description -n python3-%{name}
This package provides the Python 3 bindings for %{name}


%package -n python3-%{name}-openmpi
Summary:   %{name}  Python 3 bindings
%description -n python3-%{name}-openmpi
This package provides the Python 2 bindings for %{name}-openmpi
%endif

%if 0%{?suse_version}
%package -n python3-%{name}
Summary:   %{name}  Python 3 bindings
%{?python_provide:%python_provide python3-%{name}}
%description -n python3-%{name}
This package provides the Python 3 bindings for %{name}


%package -n python3-%{name}-openmpi
Summary:   %{name}  Python 3 bindings
%description -n python3-%{name}-openmpi
This package provides the Python 3 bindings for %{name}-openmpi
%endif



%{!?_openmpi_load: %define _openmpi_load() \
 . /etc/profile.d/modules.sh; \
 module load mpi/openmpi-%{_arch}; \
 export CFLAGS="$CFLAGS %{optflags}"; \
} 
%{!?_openmpi_unload: %define _openmpi_unload() \
 . /etc/profile.d/modules.sh; \
 module unload mpi/openmpi-%{_arch}; \
}



%prep
%setup -q
%patch0 -p1


%build

# Build serial version, dummy arguments

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
export PYTHON=%{_bindir}/python3
mkdir serial; \
cd serial; \
export CXXFLAGS=$CXXFLAGS;  export LDFLAGS=$LDFLAGS" -L/usr/%_lib/root ";\
../configure --disable-rpath  --enable-rivet=/usr  --enable-cernlib=/usr/%_lib/cernlib/2006  --enable-blackhat=$(blackhat-config --prefix)   --enable-pyext   --enable-gzip --enable-recola=/usr  --enable-hztool=/usr   --enable-hepevtsize=4000    --enable-hepmc3=/usr --enable-hepmc3root   --prefix=%{_prefix} --libdir=%{_libdir}  --enable-fastjet=/usr    --enable-openloops=/usr/%_lib/openloops --enable-hepmc2=/usr  --enable-root  --enable-binreloc   --enable-pythia --enable-lhole --enable-lhapdf=/usr;\
make -C Manual  ;\
make %{?_smp_mflags} ; \
cd ..
%endif


%if 0%{?suse_version}
export PYTHON=%{_bindir}/python3
mkdir serial; \
cd serial; \
export CXXFLAGS=$CXXFLAGS ;  export LDFLAGS=$LDFLAGS" -L/usr/%_lib/root ";\
../configure --disable-rpath  --enable-rivet=/usr  --enable-cernlib=/usr/%_lib/cernlib/2006  --enable-blackhat=$(blackhat-config --prefix)   --enable-pyext   --enable-gzip --enable-recola=/usr  --enable-hztool=/usr   --enable-hepevtsize=4000    --enable-hepmc3=/usr --disable-hepmc3root   --prefix=%{_prefix} --libdir=%{_libdir}  --enable-fastjet=/usr    --enable-openloops=/usr/%_lib/openloops --enable-hepmc2=/usr  --enable-root=/usr  --enable-binreloc   --enable-pythia --enable-lhole --enable-lhapdf=/usr;\
make -C Manual  ;\
make %{?_smp_mflags} ; \
cd ..
%endif





%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
export CC=mpicc
export CXX=mpicxx
export FC=mpif90
export F77=mpif77

# Build OpenMPI version
%{_openmpi_load}

mkdir $MPI_COMPILER; \
cd $MPI_COMPILER; \
export CXXFLAGS=$CXXFLAGS;  export LDFLAGS=$LDFLAGS" -L/usr/%_lib/root ";\
../configure --disable-rpath  --enable-rivet=/usr  --enable-cernlib=/usr/%_lib/cernlib/2006   --enable-blackhat=$(blackhat-config --prefix)    --enable-pyext   --enable-gzip  --enable-hztool=/usr --enable-hepevtsize=4000 --enable-hepmc3=/usr --enable-hepmc3root --prefix=$MPI_HOME --libdir=$MPI_HOME/%_lib  --datadir=%{_datadir}     --mandir=%{_mandir}     --infodir=%{_infodir} \
  --program-suffix=$MPI_SUFFIX  --enable-mpi --enable-recola=/usr   --enable-fastjet=/usr    --enable-openloops=/usr/%_lib/openloops --enable-hepmc2=/usr  --enable-root  --enable-binreloc   --enable-pythia --enable-lhole --enable-lhapdf=/usr;\
make -C Manual  ;\
make %{?_smp_mflags} ; \
cd ..
%{_openmpi_unload}

%endif


%if 0%{?suse_version}
export CC=mpicc
export CXX=mpicxx
export FC=mpif90
export F77=mpif77
export MPI_HOME=/usr/lib64/mpi/gcc/openmpi3
export MPI_SUFFIX=_openmpi
# Build OpenMPI version
#{_openmpi_load}
mpi-selector --set openmpi3
source /etc/profile.d/mpi-selector.sh
mkdir openmpi3; \
cd openmpi3; \
export CXXFLAGS=$CXXFLAGS;  export LDFLAGS=$LDFLAGS" -L/usr/%_lib/root ";\
../configure --disable-rpath  --enable-rivet=/usr  --enable-cernlib=/usr/%_lib/cernlib/2006   --enable-blackhat=$(blackhat-config --prefix)  --enable-pyext   --enable-gzip  --enable-hztool=/usr --enable-hepevtsize=4000 --enable-hepmc3=/usr --disable-hepmc3root --prefix=$MPI_HOME --libdir=$MPI_HOME/%_lib  --datadir=%{_datadir}     --mandir=%{_mandir}     --infodir=%{_infodir} \
  --program-suffix=$MPI_SUFFIX  --enable-mpi --enable-recola=/usr   --enable-fastjet=/usr    --enable-openloops=/usr/%_lib/openloops --enable-hepmc2=/usr  --enable-root=/usr  --enable-binreloc   --enable-pythia --enable-lhole --enable-lhapdf=/usr;\
make -C Manual  ;\
make %{?_smp_mflags} ; \
cd ..
mpi-selector --unset
#{_openmpi_unload}
%endif


%install
# Install serial version
make -C serial install DESTDIR=%{buildroot} INSTALL="install -p" CPPROG="cp -p"

# Install OpenMPI version
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} 
mkdir -p %{buildroot}/usr/%{_lib}/python%{python3_version}/
mv %{buildroot}/usr/lib/python%{python3_version}/site-packages    %{buildroot}/%{python3_sitearch}
%{_openmpi_load}
make -C $MPI_COMPILER install DESTDIR=%{buildroot} INSTALL="install -p" CPPROG="cp -p"
%{_openmpi_unload}
%endif

%if 0%{?suse_version}
mkdir -p %{buildroot}/usr/%{_lib}/python%{python3_version}/
mv %{buildroot}/usr/lib/python%{python3_version}/site-packages    %{buildroot}/%{python3_sitearch}
mpi-selector --set openmpi3
source /etc/profile.d/mpi-selector.sh
make -C openmpi3 install DESTDIR=%{buildroot} INSTALL="install -p" CPPROG="cp -p"
mpi-selector --unset
%endif

mkdir -p %{buildroot}%{_sysconfdir}/ld.so.conf.d
echo %{_libdir}/%{name} >   %{buildroot}%{_sysconfdir}/ld.so.conf.d/%{name}-%{_arch}.conf

rm -rf $RPM_BUILD_ROOT/usr/share/info/dir
export QA_RPATHS=3

%post -p /sbin/ldconfig    
%postun -p /sbin/ldconfig

%files -n %{libname}
%doc AUTHORS README COPYING
%{_libdir}/SHERPA-MC/*.so*
%{_libdir}/SHERPA-MC/*a
%{_bindir}/*
%{_sysconfdir}/ld.so.conf.d/%{name}-%{_arch}.conf

%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0}
%files  -n python3-%{name}
%{python3_sitearch}/*Sherpa*
#{python3_sitearch}/Sherpa.pyc
#{python3_sitearch}/Sherpa.pyo
#{python3_sitearch}/_Sherpa.la
#{python3_sitearch}/_Sherpa.so
#{python3_sitearch}/_Sherpa.so.0
#{python3_sitearch}/_Sherpa.so.0.0.0


%files  -n python3-%{name}-openmpi
#/usr/_lib/openmpi/lib/python{python3_version}/site-packages/Sherpa.py
#/usr/_lib/openmpi/lib/python{python3_version}/site-packages/Sherpa.pyc
#/usr/_lib/openmpi/lib/python{python3_version}/site-packages/Sherpa.pyo
#/usr/_lib/openmpi/lib/python{python3_version}/site-packages/_Sherpa.la
#/usr/_lib/openmpi/lib/python{python3_version}/site-packages/_Sherpa.so
#/usr/_lib/openmpi/lib/python{python3_version}/site-packages/_Sherpa.so.0
/usr/%_lib/openmpi/lib/python%{python3_version}/site-packages/*herpa*
%endif

%if 0%{?suse_version}

%files  -n python3-%{name}
%{python3_sitearch}/Sherpa.py
%{python3_sitearch}/_Sherpa.la
%{python3_sitearch}/_Sherpa.so
%{python3_sitearch}/_Sherpa.so.0
%{python3_sitearch}/_Sherpa.so.0.0.0


%files  -n python3-%{name}-openmpi
/usr/lib64/mpi/gcc/openmpi3/lib/python%{python3_version}/site-packages/Sherpa.py
/usr/lib64/mpi/gcc/openmpi3/lib/python%{python3_version}/site-packages/_Sherpa.la
/usr/lib64/mpi/gcc/openmpi3/lib/python%{python3_version}/site-packages/_Sherpa.so
/usr/lib64/mpi/gcc/openmpi3/lib/python%{python3_version}/site-packages/_Sherpa.so.0
/usr/lib64/mpi/gcc/openmpi3/lib/python%{python3_version}/site-packages/_Sherpa.so.0.0.0


%endif

%files -n %{sharename}
/usr/share/SHERPA-MC/*
/usr/share/man/man1/*
/usr/share/doc/*
/usr/share/info/Sherpa.info.gz
/usr/share/info/Sherpa.info-1.gz
/usr/share/info/Sherpa.info-2.gz

%files -n %{libnamedev}
%{_includedir}/SHERPA-MC/*


%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0}
%files  -n %{openmpiname} 
/usr/%_lib/openmpi/%_lib/SHERPA-MC/*.so*
/usr/%_lib/openmpi/%_lib/SHERPA-MC/*a
/usr/%_lib/openmpi/bin/*

%files -n %{openmpiname}-devel
/usr/%_lib/openmpi/include/SHERPA-MC/*

%endif

%if 0%{?suse_version}

%files  -n %{openmpiname} 
/usr/lib64/mpi/gcc/openmpi3/%_lib/SHERPA-MC/*.so*
/usr/lib64/mpi/gcc/openmpi3/%_lib/SHERPA-MC/*a
/usr/lib64/mpi/gcc/openmpi3/bin/*

%files -n %{openmpiname}-devel
/usr/lib64/mpi/gcc/openmpi3/include/SHERPA-MC/*

%endif

%changelog
* Sun Aug 01 2021 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
  - RPATH
* Wed May 26 2021 Andrii Verbytskyi 2.2.11
- Added ldconfig scripts
* Thu Nov 23 2017 Andrii Verbytskyi andrii.verbytskyi@mpp.mpg.de
+ Recola
* Thu May 26 2016 Andrii Verbytskyi 2.2.0
+ Initial spec file

