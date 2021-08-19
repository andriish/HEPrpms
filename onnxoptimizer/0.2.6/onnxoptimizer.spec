Name:           onnxoptimizer
Version:        0.2.6
Release:        1%{?dist}
Summary:        Library for C++

License:        MIT
URL:           https://github.com/onnx/optimizer
Source0:        https://github.com/onnx/optimizer/archive/refs/tags/v0.2.6.tar.gz

BuildRequires:  gcc onnx-devel onnx pybind11-devel onnx_proto
BuildRequires:  gcc-c++
BuildRequires:  cmake
Requires: onnx

%description
Some lib

%package        devel
Summary:        Development files for %{name}
Requires:       %{name}%{?_isa} = %{version}-%{release}


%description    devel
This package contains the header file for using %{name}.


%prep
%setup -q -n optimizer-%{version}
sed -i 's@set(ONNX_ROOT \${PROJECT_SOURCE_DIR}/third_party/onnx)@@g' CMakeLists.txt
sed -i 's@add_subdirectory(\${ONNX_ROOT})@find_package\(onnx\)@g' CMakeLists.txt
#sed -i 's@add_subdirectory(\${ONNX_ROOT})@find_package\(ONNX\)@g' CMakeLists.txt
sed -i 's@target_link_libraries(onnx_optimizer PUBLIC onnx)@target_link_libraries(onnx_optimizer PUBLIC onnx onnx_proto protobuf)@g' CMakeLists.txt
sed -i '/add_library(onnx_optimizer \${onnx_opt_srcs})/a target_compile_definitions(onnx_optimizer PUBLIC ONNX_ML=1 ONNX_NAMESPACE=onnx)' CMakeLists.txt


%build
%cmake  -DBUILD_ONNX_PYTHON:BOOL=ON
%cmake_build

%install
%cmake_install


%files 
%{_libdir}/*

%files devel
%{_includedir}/*


%changelog
* Fri Jan 26 2018 Andrii Verbytskyi <andrii.verbytskyi@mpp.mpg.de> 1.7.0
- start
