cmake_minimum_required (VERSION 3.13)
enable_language(Fortran)
project (mcfm C CXX Fortran)

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    #target_compile_options( mcfm PRIVATE -Wall -Wextra -Wunreachable-code -Wpedantic)
    set(CMAKE_Fortran_FLAGS "-cpp -finit-local-zero -Wall -ffixed-line-length-none -ffree-line-length-512 -Wno-unused-variable -Wno-unused-function -Wno-unused-dummy-argument -Wno-conversion -Wno-surprising -Wno-c-binding-type -Wno-real-q-constant -fopenmp")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    set(CMAKE_Fortran_FLAGS "-fpp -qopenmp -stand f08 -traceback -132 -diag-disable 1622,5268,6916,6919,7416,7343,7373,7346,7713,1789,7355,7356,7026,6243")
else()
    message( FATAL_ERROR "Unsupported Fortran compiler ${CMAKE_Fortran_COMPILER_ID}")
endif()

if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_CXX_FLAGS "-fopenmp")
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
    set(CMAKE_CXX_FLAGS "-qopenmp  -traceback")
else()
    message( FATAL_ERROR "Unsupported C++ compiler ${CMAKE_CXX_COMPILER_ID}")
endif()

if (CMAKE_C_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_C_FLAGS "-fopenmp")
elseif (CMAKE_C_COMPILER_ID STREQUAL "Intel")
    set(CMAKE_C_FLAGS "-qopenmp  -traceback")
else()
    message( FATAL_ERROR "Unsupported C compiler ${CMAKE_C_COMPILER_ID}")
endif()

### MCFM dependencies

include(ExternalProject)

ExternalProject_Add(qd
    PREFIX ${CMAKE_SOURCE_DIR}/qd-2.3.22
    SOURCE_DIR ${CMAKE_SOURCE_DIR}/qd-2.3.22
    PATCH_COMMAND mkdir -p <SOURCE_DIR>/local
    CONFIGURE_COMMAND CC=${CMAKE_C_COMPILER} CXX=${CMAKE_CXX_COMPILER} FC=${CMAKE_Fortran_COMPILER} <SOURCE_DIR>/configure --enable-fortran --enable-fma --prefix=<SOURCE_DIR>/local
    BUILD_IN_SOURCE ON
    BUILD_COMMAND ${MAKE}
    INSTALL_COMMAND ${CMAKE_MAKE_PROGRAM} install
)

ExternalProject_Add(qcdloop
    PREFIX ${CMAKE_SOURCE_DIR}/qcdloop-2.0.5
    SOURCE_DIR ${CMAKE_SOURCE_DIR}/qcdloop-2.0.5
    PATCH_COMMAND mkdir -p <SOURCE_DIR>/local
    CMAKE_ARGS -DCMAKE_BUILD_TYPE=Release -DENABLE_FORTRAN_WRAPPER=ON -DCMAKE_INSTALL_PREFIX=<SOURCE_DIR>/local -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER} -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
    BUILD_COMMAND ${MAKE}
    INSTALL_COMMAND ${CMAKE_MAKE_PROGRAM} install
)

### main MCFM code below
### main MCFM code below

add_executable(mcfm)

option(use_internal_lhapdf "Use shipped LHAPDF" ON)
option(use_external_lhapdf "Use system LHAPDF" OFF)
option(use_mpi "Use MPI" OFF)
option(use_coarray "Use Coarrays" OFF)

if(${use_internal_lhapdf} AND ${use_external_lhapdf})
    message( FATAL_ERROR "use_internal_lhapdf and use_external_lhapdf are mutually exclusive!")
endif()

if(${use_internal_lhapdf} OR ${use_external_lhapdf})
    message("Using LHAPDF!")
    set(use_lhapdf ON BOOL)
endif()

target_include_directories(mcfm PRIVATE src/Inc .)
add_subdirectory(src)
add_subdirectory(TensorReduction)


set_target_properties(mcfm PROPERTIES 
    CXX_STANDARD 11
    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include"
    LINKER_LANGUAGE Fortran)

if(use_mpi)
    add_compile_definitions(HAVE_MPI=1)
    find_package(MPI REQUIRED)
    target_link_libraries(mcfm ${MPI_Fortran_LIBRARIES})
    message("Using MPI!")
endif(use_mpi)

if (use_coarray)
    add_compile_definitions(HAVE_COARRAY=1)
        message("Using Fortran Coarrays.")
    if (use_mpi)
        message(FATAL_ERROR "MPI and Coarray support are mutually exclusive!")
    endif (use_mpi)
endif(use_coarray)

find_package(OpenMP)
if(OpenMP_Fortran_FOUND)
    target_link_libraries(mcfm ${OpenMP_Fortran_LIBRARIES})
endif()

add_library(qcdloop_lib SHARED IMPORTED)
set_property(TARGET qcdloop_lib PROPERTY IMPORTED_LOCATION "${CMAKE_SOURCE_DIR}/qcdloop-2.0.5/local/lib/libqcdloop.a")
add_dependencies(qcdloop_lib qcdloop)

add_library(qd_lib STATIC IMPORTED)
set_property(TARGET qd_lib PROPERTY IMPORTED_LOCATION "${CMAKE_SOURCE_DIR}/qd-2.3.22/local/lib/libqd.a")
add_dependencies(qd_lib qd)

add_library(qdmod_lib STATIC IMPORTED)
set_property(TARGET qdmod_lib PROPERTY IMPORTED_LOCATION "${CMAKE_SOURCE_DIR}/qd-2.3.22/local/lib/libqdmod.a")
add_dependencies(qdmod_lib qd)

if(use_internal_lhapdf)
    ExternalProject_Add(lhapdf
        URL ${CMAKE_SOURCE_DIR}/LHAPDF-6.2.3.tar.gz
        SOURCE_DIR ${CMAKE_SOURCE_DIR}/LHAPDF-6.2.3
        PATCH_COMMAND mkdir -p <SOURCE_DIR>/local
        CONFIGURE_COMMAND <SOURCE_DIR>/configure --disable-lhaglue --disable-lhaglue-cxx --disable-python --prefix=<SOURCE_DIR>/local CC=${CMAKE_C_COMPILER} CXX=${CMAKE_CXX_COMPILER}
        BUILD_COMMAND ${MAKE}
        INSTALL_COMMAND ${CMAKE_MAKE_PROGRAM} install
    )
    add_library(lhapdf_lib STATIC IMPORTED)
    set_property(TARGET lhapdf_lib PROPERTY IMPORTED_LOCATION "${CMAKE_SOURCE_DIR}/LHAPDF-6.2.3/local/lib/libLHAPDF.a")
    add_dependencies(lhapdf_lib lhapdf)
    target_link_libraries(mcfm lhapdf_lib)
    target_include_directories(mcfm PRIVATE "${CMAKE_SOURCE_DIR}/LHAPDF-6.2.3/local/include")
    add_compile_definitions(HAVE_LHAPDF=1)
elseif(use_external_lhapdf)
    find_library(lhapdf_lib NAMES LHAPDF REQUIRED)
    target_link_libraries(mcfm lhapdf_lib)
    add_compile_definitions(HAVE_LHAPDF=1)
endif()

target_link_libraries(mcfm qdmod_lib qd_lib qcdloop_lib quadmath stdc++)

install(TARGETS mcfm DESTINATION Bin)

