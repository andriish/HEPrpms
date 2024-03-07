## START: Set by rpmautospec
## (rpmautospec version 0.3.5)
## RPMAUTOSPEC: autorelease, autochangelog
%define autorelease(e:s:pb:n) %{?-p:0.}%{lua:
    release_number = 9;
    base_release_number = tonumber(rpm.expand("%{?-b*}%{!?-b:1}"));
    print(release_number + base_release_number - 1);
}%{?-e:.%{-e*}}%{?-s:.%{-s*}}%{!?-n:%{?dist}}
## END: Set by rpmautospec

%global pretty_name HighFive

%global _description %{expand:
HighFive is a modern header-only C++11 friendly interface for libhdf5.

HighFive supports STL vector/string, Boost::UBLAS, Boost::Multi-array, Eigen
and Xtensor. It handles C++ from/to HDF5 with automatic type mapping. HighFive
does not require additional libraries (see dependencies) and supports both HDF5
thread safety and Parallel HDF5 (contrary to the official hdf5 cpp)

It integrates nicely with other CMake projects by defining (and exporting) a
HighFive target.

Design:
- Simple C++-ish minimalist interface
- No other dependency than libhdf5
- Zero overhead
- Support C++11

Feature support:
- create/read/write files, datasets, attributes, groups, dataspaces.
- automatic memory management / ref counting
- automatic conversion of std::vector and nested std::vector from/to any
  dataset with basic types
- automatic conversion of std::string to/from variable length string dataset
- selection() / slice support
- parallel Read/Write operations from several nodes with Parallel HDF5
- Advanced types: Compound, Enum, Arrays of Fixed-length strings, References
  etc... (see ChangeLog)
}

%bcond_without tests
%bcond_without docs

# Header only, so no debuginfo is generated
%global debug_package %{nil}

Name:           highfive
Version:        2.3.1
Release:        2
Summary:        Header-only C++ HDF5 interface

License:        Boost
URL:            https://bluebrain.github.io/HighFive/
Source0:        https://github.com/BlueBrain/HighFive/archive/v%{version}/%{name}-%{version}.tar.gz

# Does not build on 32 bit architectures
# Issue filed upstream: https://github.com/BlueBrain/HighFive/issues/443
# https://bugzilla.redhat.com/show_bug.cgi?id=1952348
#
# Partially patched, fixing i686; see upstream bug for armv7hl status. We can
# work around this by disabling OpenCV on armv7hl until a patch is available.
Patch0:         0001-fix-32bit-arches-use-explicit-casts.patch
Patch1:         0001-Fix-compiling-invalid-reinterpret_cast-on-32-bit.patch
Patch2:         27a8f06d58a0bdb5c31a84fd8a653a9433f06082.patch

BuildRequires:  cmake
BuildRequires:  gcc-c++
BuildRequires:  git-core
BuildRequires:  hdf5-devel
# Technically optional, enabled by default
BuildRequires:  boost-devel
# Our choice vs. make
BuildRequires:  ninja-build

# Optional but included in Fedora, so we use these
BuildRequires:  eigen3-devel
%ifnarch %{arm32}
BuildRequires:  opencv-devel
%endif
# The -static versions are required by guidelines for tracking header-only
# libraries
BuildRequires:  eigen3-static


%if %{with docs}
BuildRequires:  doxygen
%endif

%description %_description


%package        devel
Summary:        Development files for %{name}
Provides:       %{name}%{?_isa} = %{version}-%{release}
Provides:       %{name}-static%{?_isa} = %{version}-%{release}
# Unarched version is needed since arched BuildRequires must not be used
Provides:       %{name}-static = %{version}-%{release}


%description    devel
The %{name}-devel package contains libraries and header files for
developing applications that use %{name}.

%if %{with docs}
%package        doc
Summary:        Documentation for %{name}
BuildArch:      noarch

%description    doc
Documentation for %{name}
%endif


%prep
%autosetup -n %{pretty_name}-%{version} -S git -p1


%build
# With g++13, warnings are generated from xtl side, e.g.
# /usr/include/xtl/xsequence.hpp:132:24: error: 'ret' may be used uninitialized
# [-Werror=maybe-uninitialized]
# Disabling -Werror
sed -i CMake/config/CompilerFlagsHelpers.cmake -e 's|-Werror ||'

%if %{with tests}
%set_build_flags
# The unit tests intentionally test deprecated APIs; silence these warnings so
# we are more likely to notice any real problems.
CXXFLAGS="${CXXFLAGS} -Wno-deprecated-declarations"
%endif
%cmake \
    -DHIGHFIVE_USE_BOOST:BOOL=TRUE \
    -DHIGHFIVE_USE_EIGEN:BOOL=TRUE \
%ifnarch %{arm32}
    -DHIGHFIVE_USE_OPENCV:BOOL=TRUE \
%endif
    -DHIGHFIVE_EXAMPLES:BOOL=TRUE \
    -DHIGHFIVE_UNIT_TESTS:BOOL=%{?with_tests:TRUE}%{?!with_tests:FALSE} \
    -DHIGHFIVE_BUILD_DOCS:BOOL=%{?with_docs:TRUE}%{?!with_docs:FALSE} \
    -GNinja
%cmake_build
%if %{with docs}
%cmake_build --target doc
%endif


%install
%cmake_install
# Move the CMake configurations to the correct location
[ ! -d '%{buildroot}/%{_libdir}/cmake/%{pretty_name}' ]
install -d '%{buildroot}/%{_libdir}/cmake'
mv -v '%{buildroot}/%{_datadir}/%{pretty_name}/CMake' \
    '%{buildroot}/%{_libdir}/cmake/%{pretty_name}'


%check
%if %{with tests}
%ctest
%endif


%files devel
%license LICENSE
%doc README.md VERSION CHANGELOG.md
%{_includedir}/%{name}
%{_libdir}/cmake/%{pretty_name}


%if %{with docs}
%files doc
%license LICENSE
%doc %{_vpath_builddir}/doc/html
%endif


%changelog
* Thu Jul 20 2023 Fedora Release Engineering <releng@fedoraproject.org> - 2.3.1-9
- Rebuilt for https://fedoraproject.org/wiki/Fedora_39_Mass_Rebuild

* Wed Mar 15 2023 Mamoru TASAKA <mtasaka@fedoraproject.org> - 2.3.1-8
- Remove -Werror to fix FTBFS, warnings are generated outside this package

* Thu Jan 19 2023 Fedora Release Engineering <releng@fedoraproject.org> - 2.3.1-7
- Rebuilt for https://fedoraproject.org/wiki/Fedora_38_Mass_Rebuild

* Thu Jul 21 2022 Fedora Release Engineering <releng@fedoraproject.org> - 2.3.1-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_37_Mass_Rebuild

* Fri Mar 18 2022 Ankur Sinha (Ankur Sinha Gmail) <sanjay.ankur@gmail.com> - 2.3.1-5
- fix: include upstream patch

* Thu Jan 20 2022 Fedora Release Engineering <releng@fedoraproject.org> - 2.3.1-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_36_Mass_Rebuild

* Mon Nov 22 2021 Orion Poplawski <orion@nwra.com> - 2.3.1-2
- Rebuild for hdf5 1.12.1

* Fri Aug 27 2021 Benjamin A. Beasley <code@musicinmybrain.net> - 2.3.1-1
- Update to 2.3.1
- Drop patches, which were all upstreamed
- Switch BR’s to cmake(…) where appropriate
- Add -static BR’s for header-only library dependencies
- Add unarched -static virtual Provides since arched BR’s must not be used
- Use ninja cmake backend (which was already BR’d)
- Enable OpenCV (except on armv7hl, for now)
- Fix ExcludeArch

* Thu Jul 22 2021 Fedora Release Engineering <releng@fedoraproject.org> - 2.2.2-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_35_Mass_Rebuild

* Thu Apr 22 2021 Ankur Sinha <ankursinha AT fedoraproject DOT org> - 2.2.2-2
- Make note of excludearch bug

* Mon Apr 19 2021 Ankur Sinha <ankursinha AT fedoraproject DOT org> - 2.2.2-2
- Carry patch to fix failing test: https://github.com/BlueBrain/HighFive/issues/444

* Sun Apr 18 2021 Ankur Sinha <ankursinha AT fedoraproject DOT org> - 2.2.2-1
- Enable doc build
- include complete doc package in conditional

