%undefine __cmake_in_source_build
%undefine __cmake3_in_source_build

Name:		HepMC3
Version:	3.2.3
Release:	3%{?dist}
Summary:	C++ Event Record for Monte Carlo Generators

License:	GPLv3+
URL:		https://hepmc.web.cern.ch/hepmc/
Source0:	https://gitlab.cern.ch/hepmc/HepMC3/-/archive/master/HepMC3-master.tar.gz

#		The ROOT cmake file used by this project requires cmake 3.9
%if %{?rhel}%{!?rhel:0} == 7
BuildRequires:	cmake3 >= 3.9
%else
BuildRequires:	cmake >= 3.9
%endif
BuildRequires:	make
BuildRequires:	gcc-c++
%ifnarch s390x
BuildRequires:	root-core
BuildRequires:	root-hist
BuildRequires:	root-io
BuildRequires:	root-tree
%endif
BuildRequires:	doxygen
BuildRequires:	graphviz
%if %{?rhel}%{!?rhel:0} == 7
BuildRequires:	python2-devel
BuildRequires:	python%{python3_other_pkgversion}-devel
%endif
BuildRequires:	python%{python3_pkgversion}-devel
#		Additional requirements for tests
BuildRequires:	pythia8-devel
BuildRequires:	valgrind
#		For HepMC2 - HepMC3 conversion tests
BuildRequires:	HepMC-devel

%description
The HepMC package is an object oriented, C++ event record for High 
Energy Physics Monte Carlo generators and simulation, described in 
A. Buckley et al., "The HepMC3 Event Record Library for Monte Carlo 
Event Generators" (Comput. Phys. Commun. (2020)),arxiv:1912.08005.
It is a continuation of the HepMC2 by M. Dobbs and J.B. Hansen described
in "The HepMC C++ Monte Carlo event record for High Energy Physics" 
(Comput. Phys. Commun. 134 (2001) 41). In the version 3  the package 
has undergone several modifications and in particular, the latest 
HepMC3 series is a completely new re-write using currently available 
C++11 techniques, and have out-of-the-box interfaces for the widely 
used in HEP community ROOT and Python.

%package devel
Summary:	C++ Event Record for Monte Carlo Generators - development files
Requires:	%{name}%{?_isa} = %{version}-%{release}

%description devel
This package provides development files for %{name}.

%package search
Summary:	C++ Event Record for Monte Carlo Generators - search engine library
Requires:	%{name}%{?_isa} = %{version}-%{release}

%description search
This package provides a library for selecting particles in HepMC3 event
records.

%package search-devel
Summary:	C++ Event Record for Monte Carlo Generators - %{name}-search development files
Requires:	%{name}-search%{?_isa} = %{version}-%{release}
Requires:	%{name}-devel%{?_isa} = %{version}-%{release}

%description search-devel
This package provides development files for %{name}-search.

%ifnarch s390x
%package rootIO
Summary:	C++ Event Record for Monte Carlo Generators - ROOT IO
Requires:	%{name}%{?_isa} = %{version}-%{release}

%description rootIO
This package provides a library for ROOT IO support.

%package rootIO-devel
Summary:	C++ Event Record for Monte Carlo Generators - %{name}-rootIO development files
Requires:	%{name}-rootIO%{?_isa} = %{version}-%{release}
Requires:	%{name}-devel%{?_isa} = %{version}-%{release}

%description rootIO-devel
This package provides development files for %{name}-rootIO.
%endif

%package interfaces-devel
Summary:	C++ Event Record for Monte Carlo Generators - generator interfaces
Requires:	%{name}-devel = %{version}-%{release}
BuildArch:	noarch

%description interfaces-devel
This package provides HepMC3 interfaces to some common Monte Carlo generators.

%if %{?rhel}%{!?rhel:0} == 7
%package -n python2-%{name}
Summary:	HeppMC3 Python 2 bindings
%{?python_provide:%python_provide python2-%{name}}
Requires:	%{name}%{?_isa} = %{version}-%{release}

%description -n python2-%{name}
This package provides the Python 2 bindings for HepMC3.

%package -n python2-%{name}-search
Summary:	HepMC3 search module Python 2 bindings
%{?python_provide:%python_provide python2-%{name}-search}
Requires:	%{name}-search%{?_isa} = %{version}-%{release}
Requires:	python2-%{name}%{?_isa} = %{version}-%{release}

%description -n python2-%{name}-search
This package provides the Python 2 bindings for HepMC3 search module.

%ifnarch s390x
%package -n python2-%{name}-rootIO
Summary:	HepMC3 ROOT I/O module Python 2 bindings
%{?python_provide:%python_provide python2-%{name}-rootIO}
Requires:	%{name}-rootIO%{?_isa} = %{version}-%{release}
Requires:	python2-%{name}%{?_isa} = %{version}-%{release}

%description -n python2-%{name}-rootIO
This package provides the Python 2 bindings for HepMC3 ROOT I/O module.
%endif

%package -n python%{python3_other_pkgversion}-%{name}
Summary:	HepMC3 Python 3 bindings
%{?python_provide:%python_provide python%{?python3_other_pkgversion}-%{name}}
Requires:	%{name}%{?_isa} = %{version}-%{release}

%description -n python%{python3_other_pkgversion}-%{name}
This package provides the Python 3 bindings for HepMC3.

%package -n python%{python3_other_pkgversion}-%{name}-search
Summary:	HepMC3 search module Python 3 bindings
%{?python_provide:%python_provide python%{?python3_other_pkgversion}-%{name}-search}
Requires:	%{name}-search%{?_isa} = %{version}-%{release}
Requires:	python%{python3_other_pkgversion}-%{name}%{?_isa} = %{version}-%{release}

%description -n python%{python3_other_pkgversion}-%{name}-search
This package provides the Python 3 bindings for HepMC3 search module.

%ifnarch s390x
%package -n python%{python3_other_pkgversion}-%{name}-rootIO
Summary:	HepMC3 ROOT I/O module Python 3 bindings
%{?python_provide:%python_provide python%{?python3_other_pkgversion}-%{name}-rootIO}
Requires:	%{name}-rootIO%{?_isa} = %{version}-%{release}
Requires:	python%{python3_other_pkgversion}-%{name}%{?_isa} = %{version}-%{release}

%description -n python%{python3_other_pkgversion}-%{name}-rootIO
This package provides the Python 3 bindings for HepMC3 ROOT I/O module.
%endif
%endif

%package -n python%{python3_pkgversion}-%{name}
Summary:	HepMC3 Python 3 bindings
%{?python_provide:%python_provide python%{python3_pkgversion}-%{name}}
Requires:	%{name}%{?_isa} = %{version}-%{release}

%description -n python%{python3_pkgversion}-%{name}
This package provides the Python 3 bindings for HepMC3.

%package -n python%{python3_pkgversion}-%{name}-search
Summary:	HepMC3 search module Python 3 bindings
%{?python_provide:%python_provide python%{python3_pkgversion}-%{name}-search}
Requires:	%{name}-search%{?_isa} = %{version}-%{release}
Requires:	python%{python3_pkgversion}-%{name}%{?_isa} = %{version}-%{release}

%description -n python%{python3_pkgversion}-%{name}-search
This package provides the Python 3 bindings for HepMC3 search module.

%ifnarch s390x
%package -n python%{python3_pkgversion}-%{name}-rootIO
Summary:	HepMC3 ROOT I/O module Python 3 bindings
%{?python_provide:%python_provide python%{python3_pkgversion}-%{name}-rootIO}
Requires:	%{name}-rootIO%{?_isa} = %{version}-%{release}
Requires:	python%{python3_pkgversion}-%{name}%{?_isa} = %{version}-%{release}

%description -n python%{python3_pkgversion}-%{name}-rootIO
This package provides the Python 3 bindings for HepMC3 ROOT I/O module.
%endif

%package doc
Summary:	C++ Event Record for Monte Carlo Generators - documentation
BuildArch:	noarch

%description doc
This package provides HepMC manuals and examples.

%prep
%setup -q -n HepMC3-master

%build
%cmake3 \
%ifnarch s390x
	-DHEPMC3_ENABLE_ROOTIO:BOOL=ON \
	-DHEPMC3_ROOTIO_INSTALL_LIBDIR:PATH=%{_libdir}/root \
%else
	-DHEPMC3_ENABLE_ROOTIO:BOOL=OFF \
%endif
	-DHEPMC3_ENABLE_TEST:BOOL=ON \
	-DHEPMC3_INSTALL_INTERFACES:BOOL=ON \
	-DHEPMC3_INSTALL_EXAMPLES:BOOL=ON \
%if %{?rhel}%{!?rhel:0} == 7
	-DHEPMC3_PYTHON_VERSIONS=2,%python3_version,%python3_other_version \
%else
	-DHEPMC3_PYTHON_VERSIONS=%python3_version \
%endif
	-DHEPMC3_BUILD_DOCS:BOOL=ON \
	-DHEPMC3_BUILD_STATIC_LIBS:BOOL=OFF \
	-DCMAKE_INSTALL_DOCDIR:PATH=%{_pkgdocdir} \
	-DCMAKE_SKIP_INSTALL_RPATH:BOOL=ON
%cmake3_build

%install
%cmake3_install

%check
%ctest3

%ldconfig_scriptlets
%ldconfig_scriptlets search
%ifnarch s390x
%ldconfig_scriptlets rootIO
%endif

%files
%{_libdir}/libHepMC3.so.3
%license COPYING

%files devel
%{_bindir}/HepMC3-config
%{_libdir}/libHepMC3.so
%dir %{_includedir}/%{name}
%dir %{_includedir}/%{name}/Data
%{_includedir}/%{name}/AssociatedParticle.h
%{_includedir}/%{name}/Attribute.h
%{_includedir}/%{name}/Data/GenEventData.h
%{_includedir}/%{name}/Data/GenParticleData.h
%{_includedir}/%{name}/Data/GenRunInfoData.h
%{_includedir}/%{name}/Data/GenVertexData.h
%{_includedir}/%{name}/Errors.h
%{_includedir}/%{name}/FourVector.h
%{_includedir}/%{name}/GenCrossSection.h
%{_includedir}/%{name}/GenCrossSection_fwd.h
%{_includedir}/%{name}/GenEvent.h
%{_includedir}/%{name}/GenHeavyIon.h
%{_includedir}/%{name}/GenHeavyIon_fwd.h
%{_includedir}/%{name}/GenParticle.h
%{_includedir}/%{name}/GenParticle_fwd.h
%{_includedir}/%{name}/GenPdfInfo.h
%{_includedir}/%{name}/GenPdfInfo_fwd.h
%{_includedir}/%{name}/GenRunInfo.h
%{_includedir}/%{name}/GenVertex.h
%{_includedir}/%{name}/GenVertex_fwd.h
%{_includedir}/%{name}/HEPEVT_Wrapper.h
%{_includedir}/%{name}/HepMC3.h
%{_includedir}/%{name}/LHEF.h
%{_includedir}/%{name}/LHEFAttributes.h
%{_includedir}/%{name}/Print.h
%{_includedir}/%{name}/PrintStreams.h
%{_includedir}/%{name}/Reader.h
%{_includedir}/%{name}/ReaderAscii.h
%{_includedir}/%{name}/ReaderAsciiHepMC2.h
%{_includedir}/%{name}/ReaderFactory.h
%{_includedir}/%{name}/ReaderHEPEVT.h
%{_includedir}/%{name}/ReaderLHEF.h
%{_includedir}/%{name}/ReaderPlugin.h
%{_includedir}/%{name}/Setup.h
%{_includedir}/%{name}/Units.h
%{_includedir}/%{name}/Version.h
%{_includedir}/%{name}/Writer.h
%{_includedir}/%{name}/WriterAscii.h
%{_includedir}/%{name}/WriterAsciiHepMC2.h
%{_includedir}/%{name}/WriterHEPEVT.h
%{_includedir}/%{name}/WriterPlugin.h
%dir %{_datadir}/%{name}
%dir %{_datadir}/%{name}/cmake
%{_datadir}/%{name}/cmake/HepMC3Config.cmake
%{_datadir}/%{name}/cmake/HepMC3Config-version.cmake

%files search
%{_libdir}/libHepMC3search.so.4

%files search-devel
%{_libdir}/libHepMC3search.so
%{_includedir}/%{name}/AttributeFeature.h
%{_includedir}/%{name}/Feature.h
%{_includedir}/%{name}/Filter.h
%{_includedir}/%{name}/FilterAttribute.h
%{_includedir}/%{name}/Relatives.h
%{_includedir}/%{name}/Selector.h

%ifnarch s390x
%files rootIO
%{_libdir}/root/libHepMC3rootIO.so.3
# unversioned symlink is used at runtime when library is used as a ROOT plugin
%{_libdir}/root/libHepMC3rootIO.so
%{_libdir}/root/libHepMC3rootIO_rdict.pcm
%{_libdir}/root/libHepMC3rootIO.rootmap

%files rootIO-devel
%{_includedir}/%{name}/ReaderRoot.h
%{_includedir}/%{name}/ReaderRootTree.h
%{_includedir}/%{name}/WriterRoot.h
%{_includedir}/%{name}/WriterRootTree.h
%endif

%files interfaces-devel
%{_datadir}/%{name}/interfaces

%if %{?rhel}%{!?rhel:0} == 7
%files -n python2-%{name}
%dir %{python2_sitearch}/pyHepMC3
%{python2_sitearch}/pyHepMC3/__init__.py*
%{python2_sitearch}/pyHepMC3/pyHepMC3.so
%{python2_sitearch}/pyHepMC3-*.egg-info

%files -n python2-%{name}-search
%dir %{python2_sitearch}/pyHepMC3/search
%{python2_sitearch}/pyHepMC3/search/__init__.py*
%{python2_sitearch}/pyHepMC3/search/pyHepMC3search.so
%{python2_sitearch}/pyHepMC3.search-*.egg-info

%ifnarch s390x
%files -n python2-%{name}-rootIO
%dir %{python2_sitearch}/pyHepMC3/rootIO
%{python2_sitearch}/pyHepMC3/rootIO/__init__.py*
%{python2_sitearch}/pyHepMC3/rootIO/pyHepMC3rootIO.so
%{python2_sitearch}/pyHepMC3.rootIO-*.egg-info
%endif

%files -n python%{python3_other_pkgversion}-%{name}
%dir %{python3_other_sitearch}/pyHepMC3
%{python3_other_sitearch}/pyHepMC3/__init__.py
%{python3_other_sitearch}/pyHepMC3/__pycache__
%{python3_other_sitearch}/pyHepMC3/pyHepMC3.so
%{python3_other_sitearch}/pyHepMC3-*.egg-info

%files -n python%{python3_other_pkgversion}-%{name}-search
%dir %{python3_other_sitearch}/pyHepMC3/search
%{python3_other_sitearch}/pyHepMC3/search/__init__.py
%{python3_other_sitearch}/pyHepMC3/search/__pycache__
%{python3_other_sitearch}/pyHepMC3/search/pyHepMC3search.so
%{python3_other_sitearch}/pyHepMC3.search-*.egg-info

%ifnarch s390x
%files -n python%{python3_other_pkgversion}-%{name}-rootIO
%dir %{python3_other_sitearch}/pyHepMC3/rootIO
%{python3_other_sitearch}/pyHepMC3/rootIO/__init__.py
%{python3_other_sitearch}/pyHepMC3/rootIO/__pycache__
%{python3_other_sitearch}/pyHepMC3/rootIO/pyHepMC3rootIO.so
%{python3_other_sitearch}/pyHepMC3.rootIO-*.egg-info
%endif
%endif

%files -n python%{python3_pkgversion}-%{name}
%dir %{python3_sitearch}/pyHepMC3
%{python3_sitearch}/pyHepMC3/__init__.py
%{python3_sitearch}/pyHepMC3/__pycache__
%{python3_sitearch}/pyHepMC3/pyHepMC3.so
%{python3_sitearch}/pyHepMC3-*.egg-info

%files -n python%{python3_pkgversion}-%{name}-search
%dir %{python3_sitearch}/pyHepMC3/search
%{python3_sitearch}/pyHepMC3/search/__init__.py
%{python3_sitearch}/pyHepMC3/search/__pycache__
%{python3_sitearch}/pyHepMC3/search/pyHepMC3search.so
%{python3_sitearch}/pyHepMC3.search-*.egg-info

%ifnarch s390x
%files -n python%{python3_pkgversion}-%{name}-rootIO
%dir %{python3_sitearch}/pyHepMC3/rootIO
%{python3_sitearch}/pyHepMC3/rootIO/__init__.py
%{python3_sitearch}/pyHepMC3/rootIO/__pycache__
%{python3_sitearch}/pyHepMC3/rootIO/pyHepMC3rootIO.so
%{python3_sitearch}/pyHepMC3.rootIO-*.egg-info
%endif

%files doc
%dir %{_pkgdocdir}
%{_pkgdocdir}/examples
%{_pkgdocdir}/html
%license COPYING

%changelog
* Tue Apr 06 2021 Mattias Ellert <mattias.ellert@physics.uu.se> - 3.2.3-3
- Rebuild for root 6.22.08

* Mon Jan 25 2021 Fedora Release Engineering <releng@fedoraproject.org> - 3.2.3-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_34_Mass_Rebuild

* Sat Dec 19 2020 Mattias Ellert <mattias.ellert@physics.uu.se> - 3.2.3-1
- Update to version 3.2.3
- Use new cmake rpm macro also for EPEL
- Fix compilation warnings
- Fix build for multiple python versions (EPEL 7) - fix from upstream git

* Mon Jul 27 2020 Fedora Release Engineering <releng@fedoraproject.org> - 3.2.2-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_33_Mass_Rebuild

* Wed Jul 22 2020 Mattias Ellert <mattias.ellert@physics.uu.se> - 3.2.2-3
- Adapt to new cmake rpm macro

* Tue Jul 14 2020 Mattias Ellert <mattias.ellert@physics.uu.se> - 3.2.2-2
- Rebuild for root 6.22.00

* Wed Jun 10 2020 Mattias Ellert <mattias.ellert@physics.uu.se> - 3.2.2-1
- Update to version 3.2.2
- Drop patches accepted upstream or previously backported
- Drop the memory reduction on ARM patch - no longer needed since the
  python module sources were split into multiple files
- Use new cmake configuration option -DHEPMC3_INSTALL_EXAMPLES and
  simplify spec file accordingly
- Bump soname for libHepMC3search.so (3 to 4)

* Tue May 26 2020 Miro Hrončok <mhroncok@redhat.com> - 3.2.1-3
- Rebuilt for Python 3.9

* Sun Mar 29 2020 Mattias Ellert <mattias.ellert@physics.uu.se> - 3.2.1-2
- Initialize ROOT in rootIO python bindings - avoids problem on EPEL 7 ppc64le
- Use upstream's fix for parallel python tests

* Sun Mar 22 2020 Mattias Ellert <mattias.ellert@physics.uu.se> - 3.2.1-1
- Update to version 3.2.1
- Drop patches accepted upstream or previously backported
- Fix glitches in the generation of the HepMC3-config script
- Add additional Python 3 version package for EPEL 7
  (cmake configuration now supports multiple Python 3 versions)
- Use new cmake configuration options -DHEPMC3_ROOTIO_INSTALL_LIBDIR and
  -DHEPMC3_BUILD_STATIC_LIBS and simplify spec file accordingly
- .egg-info filenames are now correct - auto generated provides work

* Tue Jan 28 2020 Mattias Ellert <mattias.ellert@physics.uu.se> - 3.2.0-3
- Add Python 3.9 as a valid Python version

* Tue Jan 28 2020 Fedora Release Engineering <releng@fedoraproject.org> - 3.2.0-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_32_Mass_Rebuild

* Thu Nov 28 2019 Mattias Ellert <mattias.ellert@physics.uu.se> - 3.2.0-1
- Update to version 3.2.0
- Add Python packages
- Reduce memory usage when building Python bindings on ARM

* Sat Aug 31 2019 Mattias Ellert <mattias.ellert@physics.uu.se> - 3.1.2-1
- Update to version 3.1.2
- Drop patches accepted upstream or previously backported

* Wed Jul 24 2019 Fedora Release Engineering <releng@fedoraproject.org> - 3.1.1-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_31_Mass_Rebuild

* Sun Jul 07 2019 Mattias Ellert <mattias.ellert@physics.uu.se> - 3.1.1-2
- Rebuild for root 6.18.00

* Fri Apr 05 2019 Mattias Ellert <mattias.ellert@physics.uu.se> - 3.1.1-1
- Update to version 3.1.1
- Drop patches accepted upstream or previously backported
- Fix warnings about misleading indentation
- Add missing space in installed cmake file

* Tue Mar 05 2019 Mattias Ellert <mattias.ellert@physics.uu.se> - 3.1.0-3
- Rename interfaces subpackage to interfaces-devel
- Add patch fixing installed cmake file from upstream
- Increase test timeout

* Tue Mar 05 2019 Mattias Ellert <mattias.ellert@physics.uu.se> - 3.1.0-2
- Add soversion patch from upstream

* Fri Feb 22 2019 Mattias Ellert <mattias.ellert@physics.uu.se> - 3.1.0-1
- Initial build
