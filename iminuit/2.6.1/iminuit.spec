%undefine _debugsource_packages
%global srcname iminuit

Name:           %{srcname}
Version:        2.6.1
Release:        1%{?dist}
Summary:        A Jupyter-friendly Python interface for the Minuit2 C++ library maintained by CERN’s ROOT team

License:        MIT, LGPL
URL:            https://pypi.python.org/pypi/%{srcname}
Source0:        https://github.com/scikit-hep/iminuit/archive/v%{version}.tar.gz
Source1:        https://github.com/pybind/pybind11/archive/8de7772cc72daca8e947b79b83fea46214931604.zip
Source2:        https://github.com/root-project/root/archive/907554c8a1b8a19cc4177afe1ece0f2fef0b9f79.zip

BuildRequires: gcc-c++ git cmake unzip
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
mv pybind11-8de7772cc72daca8e947b79b83fea46214931604 extern/pybind11
unzip %SOURCE2
mv root-907554c8a1b8a19cc4177afe1ece0f2fef0b9f79 extern/root
sed -i 's/3\.13/3\.11/g' CMakeLists.txt

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
* Mon May 31 2021 Andrii Verbytskyi 2.6.1
+ Version bump and first entry in changelog
