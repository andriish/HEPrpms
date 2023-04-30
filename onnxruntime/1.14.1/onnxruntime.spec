%global onnx_version 1.14.1
%global safeint_version 3.0.26

Summary:    A cross-platform inferencing and training accelerator
Name:       onnxruntime
Version:    1.14.1
Release:    1%{?dist}
# onnxruntime and SafeInt are MIT
# onnx is Apache License 2.0
# optional-lite is Boost Software License 1.0
# some protobuf helper files files are BSD (protobuf_function.cmake, pb_helper.*)
License:    MIT and ASL 2.0 and Boost and BSD
URL:        https://github.com/microsoft/onnxruntime
Source0:    https://github.com/microsoft/onnxruntime/archive/v%{version}/%{name}-%{version}.tar.gz

Patch6: patch-onnxruntime-0.txt

# MLAS is not implemented for s390x
# https://github.com/microsoft/onnxruntime/blob/master/cmake/onnxruntime_mlas.cmake#L222
# The memory exhausted when building for armv7hl
ExcludeArch:    s390x %{arm}

BuildRequires:  cmake >= 3.13
BuildRequires:  make
BuildRequires:  gcc
BuildRequires:  gcc-c++
BuildRequires:  zlib-devel
BuildRequires:  bzip2
BuildRequires:  python3-devel
BuildRequires:  python3-numpy
BuildRequires:  python3-setuptools
BuildRequires:  python3-wheel
BuildRequires:  abseil-cpp-devel
BuildRequires:  boost-devel >= 1.66
BuildRequires:  date-devel
Buildrequires:  eigen3-devel >= 1.34
BuildRequires:  flatbuffers-compiler
BuildRequires:  flatbuffers-devel
BuildRequires:  json-devel
BuildRequires:  protobuf-lite-devel
BuildRequires:  re2-devel >= 20211101
BuildRequires:  gtest-devel
BuildRequires:  gmock-devel

BuildRequires:  onnx-devel
BuildRequires:  safeint-devel

%description
%{name} is a cross-platform inferencing and training accelerator compatible
with many popular ML/DNN frameworks, including PyTorch, TensorFlow/Keras,
scikit-learn, and more.

%package devel
Summary:    The development part of the %{name} package
Requires:   %{name}%{_isa} = %{version}-%{release}

%description devel
The development part of the %{name} package

%package doc
Summary:    Documentation files for the %{name} package

%description doc
Documentation files for the %{name} package

%prep
%autosetup -p1

%build
# Re-generate flatbuffer headers
%{__python3} onnxruntime/core/flatbuffers/schema/compile_schema.py --flatc %{_bindir}/flatc

# Overrides BUILD_SHARED_LIBS flag since onnxruntime compiles individual components as static, and links
# all together into a single shared library when onnxruntime_BUILD_SHARED_LIB is ON
%cmake -Donnxruntime_BUILD_SHARED_LIB=ON \
    -Donnxruntime_DEV_MODE=OFF \
    -Donnxruntime_PREFER_SYSTEM_LIB=ON \
    -Donnxruntime_BUILD_UNIT_TESTS=OFF \
    -Donnxruntime_INSTALL_UNIT_TESTS=OFF \
    -Donnxruntime_BUILD_BENCHMARKS=OFF \
    -Donnxruntime_USE_PREINSTALLED_EIGEN=ON \
    -Donnxruntime_ENABLE_CPUINFO=OFF \
    -Deigen_SOURCE_PATH=/usr/include/eigen3 \
    -Donnxruntime_DISABLE_ABSEIL=ON \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DBUILD_SHARED_LIBS:BOOL=OFF \
    -S cmake -Donnxruntime_EXTENDED_MINIMAL_BUILD=ON -DFlatbuffers_DIR=/usr/lib64/cmake/flatbuffers/
    # -DONNX_CUSTOM_PROTOC_EXECUTABLE=/usr/bin/protoc
%cmake_build

%install
%cmake_install
mkdir -p "%{buildroot}/%{_docdir}/"
cp --preserve=timestamps -r "./docs/" "%{buildroot}/%{_docdir}/%{name}"

%check
%ctest

%files
%license LICENSE
%doc ThirdPartyNotices.txt
%{_libdir}/libonnxruntime.so.%{version}
#{_libdir}/libonnxruntime_providers_shared.so.{version}

%files devel
%dir %{_includedir}/onnxruntime/
%{_includedir}/onnxruntime/*
%{_libdir}/libonnxruntime.so
#{_libdir}/libonnxruntime_providers_shared.so
%{_libdir}/pkgconfig/libonnxruntime.pc

%files doc
%{_docdir}/%{name}

%changelog
* Mon Sep 12 2022 Alejandro Alvarez Ayllon <aalvarez@fedoraproject.org> - 1.12.1-1
- Release 1.12.1

* Wed Jan 05 2022 Alejandro Alvarez Ayllon <aalvarez@fedoraproject.org> - 1.10.0-1
- Release 1.10.0

* Wed Nov 03 2021 Alejandro Alvarez Ayllon <aalvarez@fedoraproject.org> - 1.9.1-1
- Release 1.9.1
