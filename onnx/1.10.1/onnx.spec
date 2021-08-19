#
# spec file for package python-onnx
#
# Copyright (c) 2021 SUSE LLC
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.

# Please submit bugfixes or comments via https://bugs.opensuse.org/
#


Name:           onnx
Version:        1.10.1
Release:        1%{?dist}
Summary:        Open Neural Network eXchange
License:        MIT
URL:            https://onnx.ai/
Source0:        https://github.com/onnx/onnx/archive/v%{version}.tar.gz
BuildRequires:  cmake
BuildRequires:  gcc-c++
BuildRequires:  protobuf-devel
BuildRequires:  python3-rpm-macros
Requires:       onnx == %version
Requires:       onnx_proto == %version
Requires:       onnxifi_dummy == %version
Requires:       python3-numpy 
BuildRequires:  pybind11-devel python3-devel python3-setuptools
Requires:       python3-protobuf
Requires:       python3-six
Requires:       python3-typing_extensions 
Provides:       python3-onnx-devel = %{version}-%{release}

%description
Open format to represent deep learning models. With ONNX, AI developers can
more easily move models between state-of-the-art tools and choose the
combination that is best for them. ONNX is developed and supported by a
community of partners.

%package devel
Summary:        Header files of onnx
Requires:       libonnx == %version
Requires:       libonnx_proto == %version
Requires:       libonnxifi_dummy == %version

%description  devel
Header files of ONNX.

%package -n onnxifi_dummy
Summary:        Library for ONNX Interface for Framework Integration

%description  -n  onnxifi_dummy
This package exists to create libonnx_proto, so you do no want
to install this package.


%package -n onnx_proto
Summary:        Shared library for onnx protocul bufer

%description  -n  onnx_proto
Shared library for the protocol buffer library, packaged separately to be
used by external project.

%package -n onnx-backend-test
Summary:        Test data

%description -n onnx-backend-test
This packages includes the data for testing the backend.

%package  -n python%{python3_pkgversion}-%{name}
Summary:        python bindings for %{name}
Provides:       python%{python3_pkgversion}-%{name} = %{version}-%{release}

%description -n python%{python3_pkgversion}-%{name}
python%{python3_pkgversion}-%{name} contains python bindings for %{name}.


%prep
%setup -q -n onnx-%{version}
# avoid bundles
rm -rf third_party
%autopatch -p1

%build
%cmake -DONNX_USE_PROTOBUF_SHARED_LIBS:BOOL=ON -DONNX_WERROR:BOOL=OFF -DBUILD_SHARED_LIBS:BOOL=ON
%cmake_build
%py3_build

%install
%py3_install
%cmake_install
mv %{buildroot}/%{_prefix}/lib/libonnxifi.so %{buildroot}/%{_libdir}/libonnxifi.so

%files -n python%{python3_pkgversion}-%{name}
%{_bindir}/check-model
%{_bindir}/check-node
%{_bindir}/backend-test-tools
%{python3_sitearch}/*
%doc README.md
%license LICENSE

%files -n onnx-devel
%{_includedir}/onnx
%{_libdir}/cmake/*
%exclude %{_includedir}/onnx/backend

%files -n onnx-backend-test
%{_includedir}/onnx/backend

%files -n onnxifi_dummy
%{_libdir}/libonnxifi*.so
%{_libdir}/libonnxifi_loader.*

%files -n onnx
%{_libdir}/libonnx.*

%files -n onnx_proto
%{_libdir}/libonnx_proto.so

%changelog
* Thu Aug 19 2021 AV <andrii.verbytskyi@mpp.mpg.de>
- Simplify and adapt for Fedora33 from SUSE version
* Mon Feb 15 2021 Ben Greiner <code@bnavigator.de>
- NEP 29: Tumbleweed does not have python36-numpy and depending
  packages anymore. Skip python36 build.
- Make setup.py cmake call and %%cmake macros compatible, even for
  multiple python3 flavors (when we get python39)
- Fix boo#1182258
* Fri Feb  5 2021 Guillaume GARDET <guillaume.gardet@opensuse.org>
- Update to 1.8.1: https://github.com/onnx/onnx/releases/tag/v1.8.1
  * Bug fixes:
  - #3169 To resolve memory crash on Windows, register python exceptions and update exceptions handling
  - #3171 Fix bugs in external data helpers and add add size thresholds for converting
  - #2961 Fix build issues on some distributions of linux due to hard dependency on python2
  - #3221 Fix mypy wrapper error while using ONNX as a submodule
- Changes from skipped 1.8.0: https://github.com/onnx/onnx/releases/tag/v1.8.0
  * Training
  - Added Differentiable tags to make Gradient operator better defined #2723, #2893, #2911, #2954
  - Removed GraphCall; eliminated need to implement GraphCall #2964
  - Created a tool and example for users to use TrainingInfoProto for training #3008
  * Shape Inference and Checker
  - Large model (>2GB model) support added for checker and shape_inference #2744
  - Graph level shape inference fixes to patch the IR gap introduced since IR version 4 #3023
  - Node level shape inference fixes for operators
  * Version Converter
  - More operators supported #2664
  * General Features
  - Added serialization for inputs and outputs of Sequence and Map data types #2581
  - Added programmatic access to version-table and extend make-model #2918
  - Added size check to make_tensor #2987
- Drop patch as the related problem has been fixed upstream:
  * using-onnxruntime-proto.patch
* Tue Jul 14 2020 Christian Goll <cgoll@suse.com>
- reorganized package in order to have shared library support.
  Additional  packages are:
  * onnx-devel
  * libonnxifi_dummy
  * libonnx
  * onnx-devel
  As no so versions for the shared libraries are available, there
  is not a explicit dependency for the shared libraries.
  The archive file libonnxifi_loader.a is explicitely allowed in
  the rpmlintrc as this archive file is consumed by libonnxifi_loader.so
- Added the proto files from onnxruntime so that both packages can
  use the same shared libraries:
  * added using-onnxruntime-proto.patch
* Tue Jul  7 2020 Christian Goll <cgoll@suse.com>
- updated to version 1.7.0 with following highlights:
  * Training Support, as a tech preview
  * Opset has been updated to version 12
- removed not needed patch: protobuf.patch
* Thu May 21 2020 Petr Gajdos <pgajdos@suse.com>
- %%python3_only -> %%python_alternative
* Thu Apr 23 2020 Tomáš Chvátal <tchvatal@suse.com>
- Force the interpreter to match the one during build
- Mypy should use python3 to generate stuff not python2
  * no-python2.patch
* Thu Apr 23 2020 Tomáš Chvátal <tchvatal@suse.com>
- Do not pull in py2 devel of pybind11
* Mon Mar 23 2020 Tomáš Chvátal <tchvatal@suse.com>
- Do also provide/obsolete for the devel to support migration
* Wed Mar 18 2020 Christian Goll <cgoll@suse.com>
- provide also python-onnx-devel for backcompat
* Sun Mar 15 2020 Tomáš Chvátal <tchvatal@suse.com>
- Add patch to fix build with new protobuf:
  * protobuf.patch
* Wed Mar  4 2020 Christian Goll <cgoll@suse.com>
- created just one package, as other packages require the
  source file within the package
* Tue Feb 25 2020 Tomáš Chvátal <tchvatal@suse.com>
- Add missing dependencies and enable testsuite
* Tue Feb 25 2020 Tomáš Chvátal <tchvatal@suse.com>
- Force onnx static build in cmake round to ensure we do not
  end up with unresolvable pkg
* Mon Feb 24 2020 Tomáš Chvátal <tchvatal@suse.com>
- Remove testing fluff from previous change and properly loop
  the cmake call for each python variant
- Enable python2 (well rather not explicitly disable it as there
  is no need for that at the momment)
* Mon Feb 24 2020 Tomáš Chvátal <tchvatal@suse.com>
- Force building with system cmake to respect system definitions
  and to provide debug information for later on
* Wed Jan  8 2020 Christian Goll <cgoll@suse.com>
- moved necessary defs from devel to main package
* Mon Oct 28 2019 Guillaume GARDET <guillaume.gardet@opensuse.org>
- Add python-onnx-rpmlintrc to sources
* Fri Oct 18 2019 Guillaume GARDET <guillaume.gardet@opensuse.org>
- Fix armv7 build
* Tue Oct  8 2019 Christian Goll <cgoll@suse.com>
- updated to version 1.6.0
* Fri Mar 22 2019 Christian Goll <cgoll@suse.com>
- initial commit of onnx created with py2pack
