%undefine _debugsource_packages
%global srcname iminuit
%global sum iminuit is a Jupyter-friendly Python interface for the Minuit2 C++ library maintained by CERN's ROOT team.

Name:           %{srcname}
Version:        2.4.0
Release:        1%{?dist}
Summary:        %{sum}

License:        MIT, LGPL
URL:            https://pypi.python.org/pypi/%{srcname}
Source0:        https://github.com/scikit-hep/iminuit/archive/v%{version}.tar.gz
Source1:        https://github.com/pybind/pybind11/archive/f1abf5d9159b805674197f6bc443592e631c9130.zip
Source2:        https://github.com/root-project/root/archive/a642cc22e3efa73276fb84084f9d5c2fd8dbac75.zip

BuildRequires: gcc-c++ git cmake unzip
%if 0%{?rhel} || 0%{?fedora}
BuildRequires: python%{python3_pkgversion}  python%{python3_pkgversion}-devel 
%endif
%if 0%{?suse_version}
BuildRequires: python3  python3-devel  python3-setuptools
%endif

Prefix: %{_prefix}



%description
iminuit is a Jupyter-friendly Python interface for the Minuit2 C++ library maintained by CERN's ROOT team.
Summary:        %{sum}



%prep
%autosetup -n %{srcname}-%{version}
rm -rf extern/root
rm -rf extern/pybind11
unzip %SOURCE1
mv pybind11-f1abf5d9159b805674197f6bc443592e631c9130 extern/pybind11
unzip %SOURCE2
mv root-a642cc22e3efa73276fb84084f9d5c2fd8dbac75 extern/root
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
