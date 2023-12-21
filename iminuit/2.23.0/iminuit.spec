%undefine _debugsource_packages
%global srcname iminuit
%global rootblob d0b406db5e678a2eed8eee0de3ddfdee746ea568
%global pybind11blob 914c06fb252b6cc3727d0eedab6736e88a3fcb01

Name:           %{srcname}
Version:        2.23.0
Release:        1%{?dist}
Summary:        A Jupyter-friendly Python interface for the Minuit2 C++ library maintained by CERN’s ROOT team

License:        MIT, LGPL
URL:            https://pypi.python.org/pypi/%{srcname}
Source0:        https://github.com/scikit-hep/iminuit/archive/v%{version}.tar.gz
Source1:        https://github.com/pybind/pybind11/archive/%{pybind11blob}.zip
Source2:        https://github.com/root-project/root/archive/%{rootblob}.zip

BuildRequires: gcc-c++ git cmake unzip
%if 0%{?rhel} || 0%{?fedora}
BuildRequires: python3-setuptools 
%endif
%if 0%{?rhel} || 0%{?fedora}
BuildRequires: python%{python3_pkgversion}  python%{python3_pkgversion}-devel 
%endif
%if 0%{?suse_version}
BuildRequires: python3  python3-devel  python3-setuptools
%endif

Prefix: %{_prefix}

%description
iminuit is a Jupyter-friendly Python interface for the Minuit2 C++ library maintained by CERN's ROOT team
Summary:   A Jupyter-friendly Python interface for the Minuit2 C++ library maintained by CERN’s ROOT team



%prep
%autosetup -n %{srcname}-%{version}
rm -rf extern/root
rm -rf extern/pybind11
unzip %SOURCE1
mv pybind11-%{pybind11blob} extern/pybind11
unzip %SOURCE2
mv root-%{rootblob} extern/root
sed -i 's/3\.13/3\.11/g' CMakeLists.txt
sed -i -e '1i#include <cstdint>' src/fcn.hpp

%build
%if 0%{?rhel} || 0%{?fedora}
export CMAKE_BUILD_PARALLEL_LEVEL=1
python%{python3_pkgversion} setup.py  build 
%endif
%if 0%{?suse_version}
python3 setup.py  build 
%endif



%install
%if 0%{?rhel} || 0%{?fedora}
export CMAKE_BUILD_PARALLEL_LEVEL=1
python%{python3_pkgversion}  setup.py  install -O1 --skip-build --root %{buildroot} --prefix=%{_prefix}
%endif
%if 0%{?suse_version}
python3  setup.py  install -O1 --skip-build --root %{buildroot} --prefix=%{_prefix}
%endif


# Note that there is no %%files section for the unversioned python module if we are building for several python runtimes
%files -n %{srcname}
%{python3_sitearch}/*

%changelog
* Thu Dec 21 2023 Andrii Verbytskyi <andrii.verbtskyi@mpp.mpg.de>
+ 2.23.0
* Thu Apr 27 2023 Andrii Verbytskyi 2.21.3
- Bump to 2.21.3
* Mon Aug 22 2022 Andrii Verbytskyi 2.16.0
- Bump to 2.16.0
* Mon Nov 15 2021 Andrii Verbytskyi 2.8.4
- Bump to 2.8.4
* Mon May 31 2021 Andrii Verbytskyi 2.6.1
+ Version bump and first entry in changelog
