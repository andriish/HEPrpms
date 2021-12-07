%define major       0
%define libname     PHOTOS
%define libnamedev  PHOTOS-devel
%define develnamestatic  PHOTOS-static-devel
%undefine _debugsource_packages


Name:           PHOTOS
Version:        3.61
Release:        5%{?dist}
License:        Unknown
Url:            http://photospp.web.cern.ch/photospp
Source0:        http://photospp.web.cern.ch/photospp/resources/%{name}.%{version}/%{name}.%{version}.tar.gz
Patch0:         patch-PHOTOS-0.txt
Summary:        PHOTOS Monte Carlo for bremsstrahlung in the decay of particles and resonances
Requires:       HepMC libgfortran
BuildRequires:  HepMC HepMC-devel autoconf automake libtool 
BuildRequires:  gcc-gfortran gcc-c++
Prefix: %{_prefix}
%description
Monte Carlo for bremsstrahlung in the decay of particles and
resonances




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
%patch0 -p1

%build
sed -i 's/AC_FUNC_MALLOC/#AC_FUNC_MALLOC/g' configure.in
autoreconf
%configure  --with-hepmc=/usr
make  LIBDIR=/usr/%_lib


%install
%make_install  LIBDIR=/usr/%_lib
%files 
%doc AUTHORS README COPYING
%files -n %{libname}
/usr/%_lib/libPhotospp.a  
/usr/%_lib/libPhotosppHEPEVT.a  
/usr/%_lib/libPhotosppHEPEVT.so  
/usr/%_lib/libPhotosppHEPEVT.so.1.0.0  
/usr/%_lib/libPhotosppHepMC.a  
/usr/%_lib/libPhotosppHepMC.so  
/usr/%_lib/libPhotosppHepMC.so.1.0.0  
/usr/%_lib/libPhotospp.so  
/usr/%_lib/libPhotospp.so.1.0.0

%files -n %{libnamedev}
%{_includedir}/Photos/PhotosUtilities.h
%{_includedir}/Photos/PhotosDebugRandom.h
%{_includedir}/Photos/PhotosHEPEVTEvent.h
%{_includedir}/Photos/PhotosBranch.h
%{_includedir}/Photos/PhotosHepMCEvent.h
%{_includedir}/Photos/PhotosParticle.h
%{_includedir}/Photos/forZ-MEc.h
%{_includedir}/Photos/Photos.h
%{_includedir}/Photos/PhotosEvent.h
%{_includedir}/Photos/Log.h
%{_includedir}/Photos/HEPEVT_struct.h
%{_includedir}/Photos/pairs.h
%{_includedir}/Photos/forW-MEc.h
%{_includedir}/Photos/PhotosHepMCParticle.h
%{_includedir}/Photos/PhotosHEPEVTParticle.h
%{_includedir}/Photos/photosC.h
%{_includedir}/Photos/PhotosRandom.h


%changelog
* Mon Feb 3 2014 Andrii Verbytskyi 3.54
+ Initial spec file

