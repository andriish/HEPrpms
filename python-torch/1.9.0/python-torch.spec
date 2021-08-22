%undefine _debugsource_packages

Name:           python-torch
Version:        1.9.0
Release:        1%{?dist}
Summary:        Deep learning framework pytorch/Caffe2
License:        Apache-2.0 AND BSD-2-Clause AND BSD-3-Clause AND MIT AND Zlib AND BSL-1.0
Group:          Development/Languages/Python
URL:            https://pytorch.org
Source0:        https://github.com/pytorch/pytorch/archive/v%{version}.tar.gz
Patch0:         patch-python-torch-0.txt

BuildRequires: onnx-devel FXdiv-devel FP16-devel cpuinfo cpuinfo-devel pthreadpool pthreadpool-devel python3-onnx psimd-devel  foxi-devel foxi

BuildRequires:  cmake
BuildRequires:  eigen3-devel
BuildRequires:  gcc-c++
BuildRequires:  glog-devel

BuildRequires:  leveldb-devel
BuildRequires:  numactl-devel
BuildRequires:  numactl
BuildRequires:  openblas-devel
BuildRequires:  openssl-devel
BuildRequires:  protobuf-c
BuildRequires:  protobuf-devel
BuildRequires:  python-rpm-macros
BuildRequires:  snappy-devel
#BuildRequires:  libcudnn8-devel
#BuildRequires:  libnccl-devel
Requires:       python3-future
Requires:       python3-leveldb
Requires:       python3-numpy
Requires:       python3-protobuf
Requires:       python3-six

Provides:       python3-caffe2 = %version
Provides:       python3-pytorch = %version



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
Requires:       python3-click
Requires:       python3-onnx
Requires:       python3-pip
Requires:       python3-pname

%description -n pytorch-converters
Converter from caffe2 to onnx and from caffe2 to onnx formated files.


%package -n libtorch
Summary:        Library which used by %{name}
Group:          Development/Libraries/Python

%description -n libtorch
Library which is used by %{name}

%prep
%setup -q  -D -n pytorch-%{version}
%patch0 -p1

#cp /home/andriish/Projects/TF/PATCHED/CMakeLists.txt CMakeLists.txt
#cp /home/andriish/Projects/TF/PATCHED/cmake/Dependencies.cmake cmake/Dependencies.cmake
#cp /home/andriish/Projects/TF/PATCHED/cmake/ProtoBuf.cmake cmake/ProtoBuf.cmake
#cp /home/andriish/Projects/TF/PATCHED/cmake/public/protobuf.cmake cmake/public/protobuf.cmake
#cp /home/andriish/Projects/TF/PATCHED/caffe2/CMakeLists.txt caffe2/CMakeLists.txt
#cp /home/andriish/Projects/TF/PATCHED/c10/CMakeLists.txt c10/CMakeLists.txt
#cp /home/andriish/Projects/TF/PATCHED/modules/detectron/CMakeLists.txt modules/detectron/CMakeLists.txt
#cp /home/andriish/Projects/TF/PATCHED/modules/observers/CMakeLists.txt modules/observers/CMakeLists.txt

%build
#  export USE_CUDNN=ON \

%define buildvars \
  export USE_NNPACK=OFF \
  export USE_SYSTEM_NCCL=ON \
  export PATH="/usr/local/cuda-11.2/bin:$PATH" \
  export CPLUS_INCLUDE_PATH="/usr/local/cuda-11.2/include" \
  export C_INCLUDE_PATH="/usr/local/cuda-11.2/include" \
  export LD_LIBRARY_PATH="/usr/local/cuda-11.2/lib" \
  export NCCL_INCLUDE_DIR="/usr/include/" \
  export USE_TEST=OFF \
  export USE_LEVELDB=ON \
  export USE_KINETO=0  \
  export USE_MKLDNN=0 \
  export USE_XNNPACK=OFF \
  export USE_LMDB=ON \
  export USE_SYSTEM_LIB=ON \
  export USE_SYSTEM_CPUINFO=ON \
  export USE_FBGEMM=OFF \
  export USE_SYSTEM_EIGEN_INSTALL=ON \
  export USE_SYSTEM_SLEEF=ON \
  export USE_SYSTEM_ONNX=ON \
  export USE_DISTRIBUTED=OFF \
  export USE_QNNPACK=OFF \
  export BUILD_CUSTOM_PROTOBUF=OFF \
  export USE_SYSTEM_PTHREADPOOL=ON \
  export BUILD_TEST=OFF \
  export MAX_JOBS=6  \
  export ONNX_ML=1 \
  export USE_SYSTEM_FP16=ON \
  export USE_SYSTEM_FXDIV=ON 
  export USE_SYSTEM_PSIMD=ON  
  export USE_CUDA=OFF  

#  export USE_SYSTEM_LIB="pybind11,tbb,fbgemm,fbgemm/third_party/asmjit,onnx/third_party/benchmark" \

%buildvars


CXXFLAGS="-O2 -flto=auto -ffat-lto-objects -fexceptions -g -grecord-gcc-switches -pipe -Wall -Werror=format-security -Wp,-D_FORTIFY_SOURCE=2 -Wp,-D_GLIBCXX_ASSERTIONS -specs=/usr/lib/rpm/redhat/redhat-hardened-cc1 -fstack-protector-strong   -m64 -mtune=generic -fasynchronous-unwind-tables -fstack-clash-protection -fcf-protection"
CFLAGS="-O2 -flto=auto -ffat-lto-objects -fexceptions -g -grecord-gcc-switches -pipe -Wall -Werror=format-security -Wp,-D_FORTIFY_SOURCE=2 -Wp,-D_GLIBCXX_ASSERTIONS -specs=/usr/lib/rpm/redhat/redhat-hardened-cc1 -fstack-protector-strong   -m64 -mtune=generic -fasynchronous-unwind-tables -fstack-clash-protection -fcf-protection"
export CXXFLAGS
export CFLAGS

%cmake -DUSE_NNPACK=OFF \
  -DUSE_SYSTEM_NCCL=ON \
  -DUSE_TEST=OFF \
  -DUSE_LEVELDB=ON \
  -DUSE_KINETO=0  \
  -DUSE_MKLDNN=0 \
  -DUSE_XNNPACK=OFF \
  -DUSE_LMDB=ON \
  -DUSE_SYSTEM_CPUINFO=ON \
  -DUSE_FBGEMM=OFF \
  -DUSE_SYSTEM_EIGEN_INSTALL=ON \
  -DUSE_SYSTEM_SLEEF=ON \
  -DUSE_DISTRIBUTED=OFF \
  -DUSE_QNNPACK=OFF \
  -DUSE_SYSTEM_PTHREADPOOL=ON \
  -DBUILD_CUSTOM_PROTOBUF=OFF \
  -DUSE_SYSTEM_FP16=ON \
  -DBUILD_TEST=OFF \
  -DUSE_SYSTEM_FXDIV=ON \
  -DMAX_JOBS=6     \
  -DUSE_MAGMA=OFF  -DBUILD_PYTHON=ON -DUSE_SYSTEM_ONNX=ON -DUSE_SYSTEM_FOXI=OFF \
  -DONNX_ML=2  -DONNX_NAMESPACE=onnx  -DINTERN_DISABLE_ONNX=ON  -DUSE_SYSTEM_PSIMD=ON \
  -DUSE_CUDA=OFF  -DTORCH_INSTALL_LIB_DIR=%_lib -DLIBSHM_INSTALL_LIB_SUBDIR=%_lib

%cmake_build

%install
%buildvars
%cmake_install

%files 
%{python3_sitearch}/caffe2/

%files  devel
%{_includedir}/nomnigraph/
%{_includedir}/TH/
%{_includedir}/THCUNN/
%{_includedir}/c10
%{_includedir}/ATen
#{_includedir}/pybind11
%{_includedir}/caffe2
#{_includedir}/csrc
%{_includedir}/torch
%{_includedir}/*.h
/usr/share/cmake
/usr/share/ATen

%files -n pytorch-converters
%{_bindir}/*

%files -n libtorch
%{_libdir}/*.so*
%{_libdir}/*.a

%changelog
* Fri Jan 26 2018 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 1.7.0
- start
