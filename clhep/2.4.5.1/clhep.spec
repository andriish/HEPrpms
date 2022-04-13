Summary:       Class library for High Energy Physics
Name:          clhep
Version:       2.4.5.1
Release:       1%{?dist}
License:       distributable
URL:           http://proj-clhep.web.cern.ch/proj-clhep/
Source:        https://proj-clhep.web.cern.ch/proj-clhep/dist1/clhep-%{version}.tgz
BuildRequires: binutils gcc-c++  make
Prefix:        %{_prefix}
BuildRequires: cmake >= 3.4.3

%description
The CLHEP project was proposed by Leif Lönnblad at CHEP 92. It is intended
to be a set of HEP-specific foundation and utility classes such as random
generators, physics vectors, geometry and linear algebra.

%package devel
Summary: Development files for CLHEP
Requires: %{name} = %{version}

%description devel
Install this package to develop software based on CLHEP.

%prep
%setup -q -n  %{version}/CLHEP

%build
%if 0%{?fedora}
%cmake  
%cmake_build %{?_smp_mflags} 
%endif

%if 0%{?rhel} 
mkdir -p build
cd build 
%cmake  ../
make %{?_smp_mflags} 
%endif

%if 0%{?suse_version}
%cmake
make
%endif


%install
%if 0%{?rhel} || 0%{?fedora}
cd build 
%if %{?fedora}%{!?fedora:0} 
%cmake_install 
%else
%make_install 
%endif
%endif
%if 0%{?suse_version}
%cmake_install 
%endif


%files
%{_libdir}/libCLHEP*.so

%files devel
%{_libdir}/pkgconfig/*
%{_bindir}/*
%{_includedir}/clhep.modulemap
%{_includedir}/CLHEP
%{_libdir}/libCLHEP*.a
%{_libdir}/CLHEP-%{version}/*

%changelog
* Mon Nov 15 2021 Andrii Verbytskyi 2.4.5.1
- Bump to 2.4.5.1
* Mon May 31 2021 Andrii Verbytskyi <andrii.verbtskyi@mpp.mpg.de> 
- Version bump to 2.4.4.2
* Mon May 03 2021 Andrii Verbytskyi <andrii.verbtskyi@mpp.mpg.de> 
- Added a patch to clhep-config
* Sun Feb 21 2021 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> - 2.4.4.1
- Cleanup 
* Fri Jan 08 2016 Wei-Lun Chao <bluebat@member.fsf.org> - 2.2.0.8
- Rebuild for Fedora
* Fri Mar 05 2010 Steve Huff <shuff@vecna.org> - 2.0.4.5-1 - 8792/dag
- Updated to release 2.0.4.5.
- Fixed typo in doc cleanup script.
- Split off clhep-devel.
* Mon Jun 13 2005 Wei-Lun <chaoweilun@pcmail.com.tw> - 1.8.2.1-1
- Initial spec file created.
