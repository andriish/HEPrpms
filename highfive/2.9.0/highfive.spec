%global pretty_name HighFive

%global _description %{expand:
HighFive is a modern header-only C++11 friendly interface for libhdf5.

HighFive supports STL vector/string, Boost::UBLAS, Boost::Multi-array and
Xtensor. It handles C++ from/to HDF5 with automatic type mapping. HighFive does
not require additional libraries (see dependencies).

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
  etc… (see ChangeLog)

Known flaws:
- HighFive is not thread-safe. At best it has the same limitations as the HDF5
  library. However, HighFive objects modify their members without protecting
  these writes. Users have reported that HighFive is not thread-safe even when
  using the threadsafe HDF5 library, e.g.,
  https://github.com/BlueBrain/HighFive/discussions/675.
- Eigen support in core HighFive is broken. See
  https://github.com/BlueBrain/HighFive/issues/532. H5Easy is not affected.
- The support of fixed length strings isn’t ideal.}

%bcond tests 1
# Doxygen HTML help is not suitable for packaging due to a minified JavaScript
# bundle inserted by Doxygen itself. See discussion at
# https://bugzilla.redhat.com/show_bug.cgi?id=2006555.
#
# We could enable the Doxygen PDF documentation as a substitute, but beginning
# with 2.8.0 we encounter:
#
#   ! LaTeX Error: File `topics.tex' not found.
#
# This seems like a Doxygen bug, but it’s not clear exactly what kind of bug,
# or what can be done about it.
%bcond docs 0

# Header only, so no debuginfo is generated
%global debug_package %{nil}

Name:           highfive
Version:        2.9.0
Release:        1000
Summary:        Header-only C++ HDF5 interface

# SPDX
License:        BSL-1.0
URL:            https://bluebrain.github.io/HighFive/
Source:         https://github.com/BlueBrain/HighFive/archive/v%{version}/%{name}-%{version}.tar.gz


BuildRequires:  cmake
BuildRequires:  gcc-c++
BuildRequires:  hdf5-devel

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


%prep
%setup  -q -n %{pretty_name}-%{version}

%build
%cmake \
    -DHIGHFIVE_USE_BOOST:BOOL=FALSE \
    -DHIGHFIVE_USE_XTENSOR:BOOL=FALSE \
    -DHIGHFIVE_USE_EIGEN:BOOL=FALSE \
    -DHIGHFIVE_EXAMPLES:BOOL=FALSE \
    -DHIGHFIVE_UNIT_TESTS:BOOL=FALSE \
    -DHIGHFIVE_BUILD_DOCS:BOOL=FALSE 
%cmake_build


%install
%cmake_install
# Move the CMake configurations to the correct location
[ ! -d '%{buildroot}/%{_libdir}/cmake/%{pretty_name}' ]
install -d '%{buildroot}/%{_libdir}/cmake'
mv -v '%{buildroot}/%{_datadir}/%{pretty_name}/CMake' \
    '%{buildroot}/%{_libdir}/cmake/%{pretty_name}'


%files devel
%license LICENSE
%doc README.md AUTHORS.txt CHANGELOG.md
%{_includedir}/%{name}
%{_libdir}/cmake/%{pretty_name}

%changelog
* Thu Dec 21 2023 Andrii Verbytskyi <andrii.verbtskyi@mpp.mpg.de>
+ 2.9.0
