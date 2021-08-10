#
# spec file for package python-torch
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


%define srcname pytorch
%define skip_python2 1
%define skip_python36 1
%define pname torch

%global flavor standard%{nil}

%if "%{flavor}" == "standard"
%bcond_with cuda
%endif

%if "%{flavor}" == "cuda-10-2"
%bcond_without cuda
%define cudaver 10-2
%endif
%define gloo_version 113bde13035594cafdca247be953610b53026553
%define cpuinfo_version 0e6bde92b343c5fbcfe34ecd41abf9515d54b4a7
%define sleef_version e0a003ee838b75d11763aa9c3ef17bf71a725bff
%define pybind11_version 25abf7efba0b2990f5a6dfb0a31bc65c0f2f4d17
%define onnx_version 9fdae4c68960a2d44cd1cc871c74a6a9d469fa1f
%define pthreadpool_version d465747660ecf9ebbaddf8c3db37e4a13d0c9103
%define FXdiv_version b742d1143724d646cd0f914646f1240eacf5bd73
%define psimd_version 10b4ffc6ea9e2e11668f86969586f88bc82aaefa
%define FP16_version febbb1c163726b5db24bed55cc9dc42529068997
%define gemmlowp_version 3fb5c176c17c765a3492cd2f0321b0dab712f350
%define foxi_version 97fe555430a857581b9b826ecd955e4f0a3653f0
%define QNNPACK_version 7d2a4e9931a82adc3814275b6219a03e24e36b4c
%define XNNPACK_version 7493bfb9d412e59529bcbced6a902d44cfa8ea1c

Name:           python-torch
Version:        1.5.1
Release:        6.1
Summary:        Deep learning framework aka pytorch/Caffe2
License:        Apache-2.0 AND BSD-2-Clause AND BSD-3-Clause AND MIT AND Zlib AND BSL-1.0
Group:          Development/Languages/Python
URL:            https://pytorch.org
Source0:        https://github.com/pytorch/pytorch/archive/v%{version}.tar.gz#/%{srcname}-%{version}.tar.gz
Source1:        releases.html
#License10: BSD-3-Clause
Source10:       https://github.com/facebookincubator/gloo/archive/%{gloo_version}.tar.gz#/gloo-%{gloo_version}.tar.gz
#License12: BSD-2-Clause
Source12:       https://github.com/pytorch/cpuinfo/archive/%{cpuinfo_version}.tar.gz#/cpuinfo-%{cpuinfo_version}.tar.gz
#License13: BSL-1.0
Source13:       https://github.com/zdevito/sleef/archive/%{sleef_version}.tar.gz#/sleef-%{sleef_version}.tar.gz
#License14: BSD-3-Clause
Source14:       https://github.com/pybind/pybind11/archive/%{pybind11_version}.tar.gz#/pybind11-%{pybind11_version}.tar.gz
# License15: MIT
Source15:       https://github.com/onnx/onnx/archive/%{onnx_version}.tar.gz#/onnx-%{onnx_version}.tar.gz
#License16: BSD-2-Clause
Source16:       https://github.com/Maratyszcza/pthreadpool/archive/%{pthreadpool_version}.tar.gz#/pthreadpool-%{pthreadpool_version}.tar.gz
# License17: MIT
Source17:       https://github.com/Maratyszcza/FXdiv/archive/%{FXdiv_version}.tar.gz#/FXdiv-%{FXdiv_version}.tar.gz
# License18: MIT
Source18:       https://github.com/Maratyszcza/psimd/archive/%{psimd_version}.tar.gz#/psimd-%{psimd_version}.tar.gz
# License19: MIT
Source19:       https://github.com/Maratyszcza/FP16/archive/%{FP16_version}.tar.gz#/FP16-%{FP16_version}.tar.gz
#License20: Apache-2.0
Source20:       https://github.com/google/gemmlowp/archive/%{gemmlowp_version}.tar.gz#/gemmlowp-%{gemmlowp_version}.tar.gz
#License21: MIT
Source21:       https://github.com/houseroad/foxi/archive/%{foxi_version}.tar.gz#/foxi-%{foxi_version}.tar.gz
# License22: MIT
Source22:       https://github.com/pytorch/QNNPACK/archive/%{QNNPACK_version}.tar.gz#/QNNPACK-%{QNNPACK_version}.tar.gz
# License: BSD-3-Clause
Source23:       https://github.com/google/XNNPACK/archive/%{XNNPACK_version}.tar.gz#/XNNPACK-%{XNNPACK_version}.tar.gz

Patch0:         removed-peachpy-depedency.patch
Patch1:         skip-third-party-check.patch
Patch2:         fix-call-of-onnxInitGraph.patch
Patch3:         fix-mov-operand-for-gcc.patch

# A python call to cmake fails with a return code of 1 on this arch, disable it for now.
ExcludeArch:    %ix86

BuildRequires:  %{python_module Gloo}
%ifarch x86_64
BuildRequires:  %{python_module PeachPy}
%endif
BuildRequires:  %{python_module PyYAML}
BuildRequires:  %{python_module devel}
BuildRequires:  %{python_module future}
BuildRequires:  %{python_module hypothesis}
BuildRequires:  %{python_module leveldb}
BuildRequires:  %{python_module numpy-devel}
BuildRequires:  %{python_module opcodes}
BuildRequires:  %{python_module protobuf}
BuildRequires:  %{python_module psutil}
BuildRequires:  %{python_module setuptools}
BuildRequires:  %{python_module typing_extensions}
BuildRequires:  %{python_module typing}
BuildRequires:  cmake
BuildRequires:  eigen3-devel
BuildRequires:  fdupes
BuildRequires:  gcc-c++
BuildRequires:  glog-devel
BuildRequires:  gtest
BuildRequires:  leveldb-devel
BuildRequires:  libnuma-devel
BuildRequires:  libopenblas_pthreads-devel
BuildRequires:  lmdb-devel
BuildRequires:  ninja
BuildRequires:  openblas-devel
BuildRequires:  openssl-devel
BuildRequires:  protobuf-c
BuildRequires:  protobuf-devel
BuildRequires:  python-rpm-macros
BuildRequires:  snappy-devel
%if %{with cuda}
BuildRequires:  cuda-compiler-%cudaver
BuildRequires:  cuda-cudart-dev-%cudaver
BuildRequires:  cuda-libraries-dev-%cudaver
BuildRequires:  cuda-misc-headers-%cudaver
BuildRequires:  cuda-nsight-%cudaver
BuildRequires:  cuda-toolkit-%cudaver
%if 0%{?suse_version} > 1500
BuildRequires:  gcc7
BuildRequires:  gcc7-c++
%endif
BuildRequires:  libcudnn7-devel
BuildRequires:  libnccl-devel
%endif
Requires:       python-future
Requires:       python-leveldb
Requires:       python-numpy
Requires:       python-protobuf
Requires:       python-six

Provides:       python-caffe2 = %version
Provides:       python-pytorch = %version

%if "%flavor" == ""
ExclusiveArch:  do_not_build
%endif

%python_subpackages

%description
PyTorch enables fast, flexible experimentation and efficient production through
a hybrid front-end, distributed training, and ecosystem of tools and libraries.
The library is developed by Facebook and other groups.
PyTorch provides two high-level features:
* Tensor computing (like NumPy) with strong acceleration via graphics
* processing units (GPU) Deep neural networks built on a tape-based autodiff
  system

%package devel
Summary:        Headers for C/C++, cmake build description and libraries needed for development
Group:          Development/Languages/Python
Requires:       python-torch = %{version}

%description devel
Although the Python interface is more polished and the primary focus of
development, PyTorch also has a C++ frontend. This package contains the header
to access the C/C++ interface.

%package -n pytorch-converters
Summary:        Converters for onnx and caffe2
Group:          Development/Languages/Python
BuildArch:      noarch
Requires:       python3-click
Requires:       python3-onnx
Requires:       python3-pip
Requires:       python3-pname

%description -n pytorch-converters
Converter from caffe2 to onnx and from caffe2 to onnx formated files.

%package -n pytorch-examples
Summary:        Examples which can be used for testing
Group:          Development/Languages/Python
BuildArch:      noarch
Recommends:     python3-lmdb
Recommends:     python3-networkx

%description -n pytorch-examples
This example files can be used to start an own pytorch/caffe2 project.

%package -n libtorch
Summary:        Library which used by %{name}
Group:          Development/Libraries/Python

%description -n libtorch
Library which is used by %{name}

%prep
%define make_depend_src() test -e $(basename %1| sed 's/-.*//') && rmdir %{?2}%{!?2:$(basename %1| sed 's/-.*//')}; tar xzf %1; mv $(basename %1 | sed 's/\.tar\.gz//' ) %{?2}%{!?2:$(basename %1| sed 's/-.*//')}
%define make_depend_src_uppercase() rmdir -p $(basename %1| sed 's/-.*//'| tr '[:upper:]' '[:lower:]'); tar xzf %1; mv $(basename %1 | cut -f 1 -d '.' ) $(basename %1| sed 's/-.*//'| tr '[:upper:]' '[:lower:]')
%setup -q -n %{srcname}-%{version}
cp %{S:1} releases.html
%autopatch -p 1
cd third_party
rmdir python-peachpy/
%make_depend_src %{SOURCE10}
%make_depend_src %{SOURCE12}
%make_depend_src %{SOURCE13}
%make_depend_src %{SOURCE14}
%make_depend_src %{SOURCE15}
%make_depend_src %{SOURCE16}
%make_depend_src %{SOURCE17}
%make_depend_src %{SOURCE18}
%make_depend_src %{SOURCE19}
%make_depend_src %{SOURCE20} gemmlowp/gemmlowp
%make_depend_src %{SOURCE21}
%make_depend_src %{SOURCE22}
%make_depend_src %{SOURCE23}

%build
%define buildvars \
  export USE_NNPACK=OFF \
  %if %{with cuda} \
  export USE_CUDNN=ON \
  export USE_SYSTEM_NCCL=ON \
  export PATH="/usr/local/cuda-10.1/bin:$PATH" \
  export CPLUS_INCLUDE_PATH="/usr/local/cuda-10.1/include" \
  export C_INCLUDE_PATH="/usr/local/cuda-10.1/include" \
  export LD_LIBRARY_PATH="/usr/local/cuda-10.1/lib" \
  export NCCL_INCLUDE_DIR="/usr/include/" \
  %if 0%{?suse_version} > 1500  \
  export CC=gcc-7 \
  export CXX=g++-7 \
  %endif \
  %else \
  export USE_CUDNN=OFF \
  %endif \
  export USE_TEST=OFF \
  export USE_LEVELDB=ON \
  export USE_LMDB=ON \
  export USE_FBGEMM=OFF \
  export USE_SYSTEM_LIB="tbb,fbgemm,fbgemm/third_party/asmjit,onnx/third_party/benchmark" \
  export USE_SYSTEM_EIGEN_INSTALL=ON \
  export BUILD_CUSTOM_PROTOBUF=OFF \
  export BUILD_TEST=OFF \
  export MAX_JOBS=%{?jobs} \

%buildvars
%python_build

%install
%buildvars
%python_install

%python_expand %fdupes %{buildroot}%{$python_sitearch}

install -m 755 -D caffe2/python/examples/* -t %{buildroot}%{_docdir}/%{name}/
install -m 644 -D %{buildroot}%{python_sitearch}/torch/lib/* %{buildroot}/%{_libdir}
#rm -r %{buildroot}%{python_sitearch}/torch/lib
#cd %{buildroot}/%{_libdir}
#rm libtorch.so
#ln -s libtorch.so.1 libtorch.so
#cd -
#for file in  $(find %{buildroot}%{python_sitearch} -type f -name \*.py -perm 644 -size +1b); do
#%{__grep} '/usr/bin/env ' $file && sed -i 's@/usr/bin/env python@/usr/bin/python@' $file && chmod 755 $file
#done
#
#%check
#export LD_LIBRARY_PATH=%{buildroot}/%{_libdir}
#%%python_expand PYTHONPATH=%{buildroot}%{$python_sitearch} $python test/run_test.py

%post -n libtorch -p /sbin/ldconfig
%postun -n libtorch -p /sbin/ldconfig

%files %{python_files}
%defattr(-,root,root)
%doc README.md NOTICE releases.html
%license LICENSE
%{python_sitearch}/torch/
%{python_sitearch}/caffe2/
%{python_sitearch}/torch-*.egg-info/
# excluding nearly all headersm except THNN.h and THCUNN.h as they
# are read in by the python init
%exclude %{python_sitearch}/torch/share
%exclude %{python_sitearch}/torch/include/TH
%exclude %{python_sitearch}/torch/include/c10
%exclude %{python_sitearch}/torch/include/ATen
%exclude %{python_sitearch}/torch/include/pybind11
%exclude %{python_sitearch}/torch/include/caffe2
%exclude %{python_sitearch}/torch/include/torch/csrc
%exclude %{python_sitearch}/torch/include/torch/*.h

%files %{python_files devel}
%{python_sitearch}/torch/share
%{python_sitearch}/torch/include/TH/
%{python_sitearch}/torch/include/c10
%{python_sitearch}/torch/include/ATen
%{python_sitearch}/torch/include/pybind11
%{python_sitearch}/torch/include/caffe2
%{python_sitearch}/torch/include/torch/csrc
%{python_sitearch}/torch/include/torch/*.h

%files -n pytorch-converters
%{_bindir}/convert-caffe2-to-onnx
%{_bindir}/convert-onnx-to-caffe2

%files -n pytorch-examples
%{_docdir}/%{name}

%files -n libtorch
%{_libdir}/*.so*

%changelog
* Thu Jul 22 2021 Guillaume GARDET <guillaume.gardet@opensuse.org>
- Add _service file to ease future update of deps
* Thu Jul 22 2021 Guillaume GARDET <guillaume.gardet@opensuse.org>
- Update sleef to fix build on aarch64
* Fri Apr 23 2021 Matej Cepl <mcepl@suse.com>
- Don't build python36-* package (missing pandas)
* Thu Jan 21 2021 Benjamin Greiner <code@bnavigator.de>
- Fix python-rpm-macros usage
* Wed Oct  7 2020 Guillaume GARDET <guillaume.gardet@opensuse.org>
- Use GCC9 to build on aarch64 Tumbleweed to workaround SVE
  problem with GCC10 with sleef, see:
  https://github.com/pytorch/pytorch/issues/45971
* Thu Aug 20 2020 Martin Liška <mliska@suse.cz>
- Use memoryperjob constraint instead of %%limit_build macro.
* Tue Jun 23 2020 Christian Goll <cgoll@suse.com>
- updated to new stable release 1.5.1 which has following changes:
  This release includes several major new API additions and improvements. These
  include new APIs for autograd allowing for easy computation of hessians and
  jacobians, a significant update to the C++ frontend, ‘channels last’ memory
  format for more performant computer vision models, a stable release of the
  distributed RPC framework used for model parallel training, and a new API
  that allows for the creation of Custom C++ Classes that was inspired by
  PyBind. Additionally torch_xla 1.5 is now available and tested with the
  PyTorch 1.5 release providing a mature Cloud TPU experience.
  * see release.html for detailed information
- added patches:
  * fix-call-of-onnxInitGraph.patch for API mismatch in onnx
  * fix-mov-operand-for-gcc.patch for aarch64 operands
- removed sources:
  * cpuinfo-89fe1695edf9ee14c22f815f24bac45577a4f135.tar.gz
  * gloo-7c541247a6fa49e5938e304ab93b6da661823d0f.tar.gz
  * onnx-fea8568cac61a482ed208748fdc0e1a8e47f62f5.tar.gz
  * psimd-90a938f30ba414ada2f4b00674ee9631d7d85e19.tar.gz
  * pthreadpool-13da0b4c21d17f94150713366420baaf1b5a46f4.tar.gz
- added sources:
  * cpuinfo-0e6bde92b343c5fbcfe34ecd41abf9515d54b4a7.tar.gz
  * gloo-113bde13035594cafdca247be953610b53026553.tar.gz
  * onnx-9fdae4c68960a2d44cd1cc871c74a6a9d469fa1f.tar.gz
  * psimd-10b4ffc6ea9e2e11668f86969586f88bc82aaefa.tar.gz
  * pthreadpool-d465747660ecf9ebbaddf8c3db37e4a13d0c9103.tar.gz
* Tue Jun 23 2020 Christian Goll <cgoll@suse.com>
- updated to bugfix release 1.4.1 and added _multibuild file so
  that cuda versions can be build on commandline
* Thu Apr 23 2020 Tomáš Chvátal <tchvatal@suse.com>
- Make sure to pull py2/py3 package from the devel pkg
* Thu Apr 23 2020 Tomáš Chvátal <tchvatal@suse.com>
- Do not pull in python2 only dependencies
* Wed Feb 26 2020 Simon Lees <sflees@suse.de>
- Exclude i586 builds for now, they fail with a cryptic return
  code of 1 from cmake from python.
* Fri Feb 21 2020 Christian Goll <cgoll@suse.com>
- updated to stable release 1.4.0, which has as Highlights:
  * Distributed Model Parallel Training
  * Pruning functionalities have been added to PyTorch
- New Features:
  * torch.optim.lr_scheduler now support “chaining.”
  * torch.distributed.rpc is a newly introduced package
- full Changelog listed in relases file or under
  https://github.com/pytorch/pytorch/releases
  and in the releases.hml file
- added files:
  * skip-third-party-check.patch which is a patch to skip
    the check of disabled dependencies
  * QNNPACK-7d2a4e9931a82adc3814275b6219a03e24e36b4c.tar.gz
    which is part of pytorch but developed in different repo
  * releases.html which is the downloaded releases file
- removed patch files:
  * fix-build-options.patch
  * honor-PSIMD-env.patch
  * removed-some-tests.patch
* Tue Jan 14 2020 Guillaume GARDET <guillaume.gardet@opensuse.org>
- Requires python-PeachPy on x86_64 only, as it is optional
  and available on x86_64 only
* Wed Jan  8 2020 Christian Goll <cgoll@suse.com>
- updated the requirement for examples and converters
* Wed Jun 12 2019 Christian Goll <cgoll@suse.com>
- Updated to stable version 1.1.0, which needed also updates of
  following dependend sources:
  * onnx-1.4.1.tar.gz ->
    onnx-22662bfd4dcc6baebf29e3b823a051676f991001.tar.gz
- Removed following sources:
  * FBGEMM-f65f0ebe54f0512d8f42ee10025b596e3f42e0b8.tar.gz
- Added following sources:
  * foxi-8f74bc4df3a4cfc69b1a3eadf62aa29d9961c72d.tar.gz
- Changed patch
  * fix-build-options.patch to work with new buid system and
    exclude FBGEMM
- Added patch:
  * honor-PSIMD-env.patch, which makes depend sources of pytorch
    to use the source of psimd
* Tue Mar 26 2019 Christian Goll <cgoll@suse.com>
- Inital commit of pytorch/caffe2 which is an opensource
  machineleraning platform. This is the stable release 1.0.1
  including like other tools a lot of third party sources,
  which could not be used from the base system due to messy
  build system. Additional sources are
  * gloo, a communitcation library for GPUs as
    gloo-670b4d4aa46886cc66874e2a4dc846f5cfc2a285.tar.gz
  * fbgemm, a low precission, high peformance matrix lib
    FBGEMM-f65f0ebe54f0512d8f42ee10025b596e3f42e0b8.tar.gz
  * cpuinfo, a cross platform cpu information tool
    cpuinfo-89fe1695edf9ee14c22f815f24bac45577a4f135.tar.gz
  * sleef, a function for elementary functions
    sleef-191f655caa25526ae226cf88dd2529265176014a.tar.gz
  * pytbind11, which exposes C/C++ headers to pythob, but
    the source code of this library is deeply integrated into
    pytorch, so we need
    pybind11-25abf7efba0b2990f5a6dfb0a31bc65c0f2f4d17.tar.gz
  * onnx, which is an format for exchaning neural networks as
    onnx-1.4.1.tar.gz
  * pthreadpool, a pthread based thread tool implementation, which
    can be used when omp is not available
    pthreadpool-13da0b4c21d17f94150713366420baaf1b5a46f4.tar.gz
  * FXdiv, a Header-only library for division via fixed-point
    multiplication by inverse, which has no stable API atm, so
    FXdiv-b742d1143724d646cd0f914646f1240eacf5bd73.tar.gz
  * psimd, portable 128-bit SIMD intrinsics
    psimd-90a938f30ba414ada2f4b00674ee9631d7d85e19.tar.gz
  * fp16, a numeric conversion library
    FP16-febbb1c163726b5db24bed55cc9dc42529068997.tar.gz
  * gemmlowp, self-contained low-precision GEMM library as
    gemmlowp-8416bab644641a5c0a81ecf91a5cda804af0aee1.tar.gz
  * fix-build-options.patch, which points pytorch to system libs
  * removed-peachpy-depedency.patch, which forces to use system
    peachpy
