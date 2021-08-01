%undefine _debugsource_packages
%global srcname awkward

Name:           python-%{srcname}
Version:        1.4.0
Release:        1%{?dist}
Summary:        A library for nested, variable-sized data, including arbitrary-length lists, records, mixed types, and missing data, using NumPy-like idioms.

License:        MIT
URL:            https://pypi.python.org/pypi/example
Source0:        https://github.com/scikit-hep/awkward-1.0/archive/%{version}.tar.gz
Source1:        https://github.com/pybind/pybind11/archive/8de7772cc72daca8e947b79b83fea46214931604.zip
Source2:        https://github.com/Tencent/rapidjson/archive/f54b0e47a08782a6131cc3d60f94d038fa6e0a51.zip
Source3:        https://github.com/dmlc/dlpack/archive/e1e11e0d555c08bec08a6c7773aa777dfcaae9da.zip
Patch0:         patch-awkward-0.txt
%global _description %{expand:
a library for nested, variable-sized data, including arbitrary-length lists, records, mixed types, and missing data, using NumPy-like idioms.}

%description %_description


#BuildRequired: rapidjson-devel dlpack-devel pybind11-devel

%package -n python3-%{srcname}
Summary:        %{summary}
BuildRequires: cmake make gcc-c++
%if 0%{?rhel} || 0%{?fedora}
#BuildRequires: rapidjson-devel dlpack-devel pybind11-devel
BuildRequires:  python3-devel
BuildRequires:  python3-setuptools
BuildRequires:  python3-pyyaml
%endif
%if 0%{?suse_version}
#BuildRequires: rapidjson-devel dlpack-devel python-pybind11-common-devel
BuildRequires:  fdupes
BuildRequires:  python-rpm-macros
BuildRequires:  python3
BuildRequires:  python3-devel
BuildRequires:  python3-setuptools
BuildRequires:  python3-pyyaml
%endif

%description -n python3-%{srcname} %_description

%prep
%autosetup -n %{srcname}-1.0-%{version}
rm -rf pybind11
unzip %SOURCE1
mv pybind11-8de7772cc72daca8e947b79b83fea46214931604 pybind11

rm -rf rapidjson
unzip %SOURCE2
mv rapidjson-f54b0e47a08782a6131cc3d60f94d038fa6e0a51 rapidjson

rm -rf dlpack
unzip %SOURCE3
mv dlpack-e1e11e0d555c08bec08a6c7773aa777dfcaae9da dlpack


%build
%py3_build

%install
%py3_install



# Note that there is no %%files section for the unversioned python module
%files -n python3-%{srcname}

%{python3_sitearch}/awkward-*.egg-info/
%{python3_sitearch}/awkward/
%{python3_sitearch}/*.so

%changelog
* Sun Aug 01 2021 Andrii Verbytskyi 1.4
- First version 1.4
