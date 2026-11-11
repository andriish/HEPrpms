%undefine _debugsource_packages
%global upstream_ver 0.6-5
Summary: PHIGS (Programmer's Hierarchical Interactive Graphics System) library with an OpenGL backend
Name: OpenPHIGS
Version: 0.6.5
Release: 1%{?dist}
License: LGPL-2.1-only
URL: https://github.com/CERN/OpenPHIGS
Source0: https://github.com/CERN/OpenPHIGS/archive/refs/tags/v%{upstream_ver}.tar.gz

BuildRequires: cmake >= 3.5
BuildRequires: gcc gcc-c++
%if 0%{?suse_version}
BuildRequires: gcc-fortran
BuildRequires: Mesa-libGLU-devel
BuildRequires: libpng16-devel
%else
BuildRequires: gcc-gfortran
BuildRequires: mesa-libGLU-devel
BuildRequires: libpng-devel
%endif
BuildRequires: libX11-devel libXt-devel libXmu-devel libXaw-devel
%if %{?rhel}%{!?rhel:0} == 8 || %{?rhel}%{!?rhel:0} == 9
BuildRequires: openmotif-devel
%else
BuildRequires: motif-devel
%endif
BuildRequires: glew-devel
BuildRequires: gl2ps-devel

%description
OpenPHIGS is an implementation of the PHIGS 3D graphics standard with C and Fortran 77 bindings
and an OpenGL backend. It is based on the OpenPHIGS from SourceForge and contains modifications and
extensions needed for data preservation purposes in high energy physics, e.g. to display the events of
the DELPHI and OPAL experiments. The implementation does not cover the full standard.

%package devel
Summary: Development files for %{name}
Requires: %{name}%{?_isa} = %{version}-%{release}
Requires: libX11-devel libXt-devel libXmu-devel libXaw-devel
%if %{?rhel}%{!?rhel:0} == 8 || %{?rhel}%{!?rhel:0} == 9
Requires: openmotif-devel
%else
Requires: motif-devel
%endif
Requires: glew-devel gl2ps-devel
%if 0%{?suse_version}
Requires: Mesa-libGLU-devel
%else
Requires: mesa-libGLU-devel
%endif
Provides: %{name}-static = %{version}-%{release}

%description devel
Header files and the static library needed to develop applications that use %{name}.

%package examples
Summary: Example and test programs for %{name}
Requires: %{name}%{?_isa} = %{version}-%{release}

%description examples
Test programs written in C and Fortran 77 that exercise the %{name} library.

%prep
%setup -q -n %{name}-%{upstream_ver}

%build
# The upstream build system selects GLEW or Epoxy with USE_GLEW; the shaders are generated at build time.
export CFLAGS="%{optflags} -std=gnu17 -Wno-incompatible-pointer-types -Wno-int-conversion"
%cmake -S src -DUSE_GLEW=1
%cmake_build

%install
%cmake_install
# Upstream installs everything relative to the prefix, so the static library and the tests need to be relocated.
if [ -d %{buildroot}%{_prefix}/lib ] && [ "%{_libdir}" != "%{_prefix}/lib" ]; then
  mkdir -p %{buildroot}%{_libdir}
  mv %{buildroot}%{_prefix}/lib/libphigs.a %{buildroot}%{_libdir}/
  rmdir %{buildroot}%{_prefix}/lib 2>/dev/null || true
fi
mkdir -p %{buildroot}%{_libdir}/%{name}
mv %{buildroot}%{_prefix}/tests_c %{buildroot}%{_libdir}/%{name}/tests_c
mv %{buildroot}%{_prefix}/tests_f %{buildroot}%{_libdir}/%{name}/tests_f

%files
%license share/doc/lgpl-2.1.txt
%doc README.md CHANGELOG BUGS.txt Contributors.txt
%{_bindir}/ardisplay
%{_bindir}/objdisplay
%{_libdir}/libphigs.so

%files devel
%{_includedir}/phigs
%{_libdir}/libphigs.a

%files examples
%{_libdir}/%{name}

%changelog
* Wed Nov 11 2026 Andrii Verbytskyi - 0.6.5-1
- Initial packaging of OpenPHIGS v0.6-5
