cmake_minimum_required (VERSION 3.13)
enable_language(Fortran)
project (mcfm C CXX Fortran)

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp -finit-local-zero -ffixed-line-length-none -ffree-line-length-512 -fopenmp -ffpe-summary=none")

    if (CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL "10.0")
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch")
    endif()

elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpp -qopenmp -stand f08 -traceback -132 -diag-disable 1622,5268,6916,6919,7416,7343,7373,7346,7713,1789,7355,7356,7026,6243")
    message( FATAL_ERROR "Intel compiler is not supported, use at your own risk!" )
else()
    message( FATAL_ERROR "Unsupported Fortran compiler ${CMAKE_Fortran_COMPILER_ID}")
endif()

if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fopenmp")
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -qopenmp  -traceback")
else()
    message( FATAL_ERROR "Unsupported C++ compiler ${CMAKE_CXX_COMPILER_ID}")
endif()

if (CMAKE_C_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fopenmp")
elseif (CMAKE_C_COMPILER_ID STREQUAL "Intel")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -qopenmp  -traceback")
else()
    message( FATAL_ERROR "Unsupported C compiler ${CMAKE_C_COMPILER_ID}")
endif()

set(default_build_type "RelWithDebInfo")
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to '${default_build_type}' as none was specified.")
  set(CMAKE_BUILD_TYPE "${default_build_type}" CACHE
      STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
    "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

### MCFM dependencies

include(ExternalProject)

ExternalProject_Add(qd
    DOWNLOAD_COMMAND mkdir -p ${CMAKE_BINARY_DIR}/lib && cp -r ${CMAKE_SOURCE_DIR}/lib/qd-2.3.22 ${CMAKE_BINARY_DIR}/lib/
    PREFIX ${CMAKE_BINARY_DIR}/lib/qd-2.3.22
    SOURCE_DIR ${CMAKE_BINARY_DIR}/lib/qd-2.3.22
    BUILD_IN_SOURCE ON
    PATCH_COMMAND mkdir -p ${CMAKE_BINARY_DIR}/local
    CONFIGURE_COMMAND CC=${CMAKE_C_COMPILER} CXX=${CMAKE_CXX_COMPILER} FC=${CMAKE_Fortran_COMPILER} ./configure --with-pic --enable-fortran --enable-fma --prefix=${CMAKE_BINARY_DIR}/local --enable-shared --libdir=${CMAKE_BINARY_DIR}/local/lib
    BUILD_COMMAND ${MAKE}
    INSTALL_COMMAND ${CMAKE_MAKE_PROGRAM} install
)

ExternalProject_Add(qcdloop
    DOWNLOAD_COMMAND mkdir -p ${CMAKE_BINARY_DIR}/lib && cp -r ${CMAKE_SOURCE_DIR}/lib/qcdloop-2.0.9 ${CMAKE_BINARY_DIR}/lib
    PREFIX ${CMAKE_BINARY_DIR}/lib/qcdloop-2.0.9
    SOURCE_DIR ${CMAKE_BINARY_DIR}/lib/qcdloop-2.0.9
    BUILD_IN_SOURCE ON
    PATCH_COMMAND mkdir -p ${CMAKE_BINARY_DIR}/local
    CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} -DENABLE_FORTRAN_WRAPPER=ON -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/local -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER} -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER} -DCMAKE_INSTALL_LIBDIR=lib
    BUILD_COMMAND ${MAKE}
    INSTALL_COMMAND ${CMAKE_MAKE_PROGRAM} install
)

ExternalProject_Add(liboneloop
    DOWNLOAD_COMMAND mkdir -p ${CMAKE_BINARY_DIR}/lib && cp -r ${CMAKE_SOURCE_DIR}/lib/oneloop ${CMAKE_BINARY_DIR}/lib/
    PREFIX ${CMAKE_BINARY_DIR}/lib/oneloop
    SOURCE_DIR ${CMAKE_BINARY_DIR}/lib/oneloop
    BUILD_IN_SOURCE ON
    CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER} -DCMAKE_Fortran_FLAGS=-fPIC
    BUILD_COMMAND ${MAKE}
    INSTALL_COMMAND mkdir -p ${CMAKE_BINARY_DIR}/local/include && cp ${CMAKE_BINARY_DIR}/lib/oneloop/avh_olo.mod ${CMAKE_BINARY_DIR}/local/include 
)

ExternalProject_Add(handyg
    DOWNLOAD_COMMAND mkdir -p ${CMAKE_BINARY_DIR}/lib && cp -r ${CMAKE_SOURCE_DIR}/lib/handyG ${CMAKE_BINARY_DIR}/lib/
    PREFIX ${CMAKE_BINARY_DIR}/lib/handyG
    SOURCE_DIR ${CMAKE_BINARY_DIR}/lib/handyG
    BUILD_IN_SOURCE ON
    PATCH_COMMAND mkdir -p ${CMAKE_BINARY_DIR}/local
    CONFIGURE_COMMAND CC=${CMAKE_C_COMPILER} CXX=${CMAKE_CXX_COMPILER} FC=${CMAKE_Fortran_COMPILER} CFLAGS=-fPIC CXXFLAGS=-fPIC FFLAGS=-fPIC  ./configure --prefix=${CMAKE_BINARY_DIR}/local --moduledir=${CMAKE_BINARY_DIR}/local/include
    BUILD_COMMAND ${MAKE}
    INSTALL_COMMAND ${CMAKE_MAKE_PROGRAM} install
)

## Configuration options

option(use_internal_lhapdf "Use shipped LHAPDF" ON)
option(use_mpi "Use MPI" OFF)
option(use_coarray "Use Coarrays" OFF)
option(with_library "Build MCFM as library" OFF)
option(lhapdf_include_path "LHAPDF include path" OFF)

option(with_vvamp "Build with VVAmp for WW,WZ and ZZ processes" ON)

string(ASCII 27 Esc)
set(BoldRed "${Esc}[1;31m")
set(ColorReset "${Esc}[m")
if(NOT ${with_vvamp})
    message("${BoldRed}")
    message("WARNING: Building without support for WW,WZ and ZZ processes at NNLO.")
    message("To enable, build with -Dwith_vvamp=ON (but takes significantly longer).")
    message("${ColorReset}")
endif()

if(${use_internal_lhapdf})
    set(use_external_lhapdf OFF BOOL)
else()
    set(use_external_lhapdf ON BOOL)
endif()

add_compile_definitions(HAVE_LHAPDF=1)

### main MCFM code below

add_executable(mcfm)
add_library(objlib OBJECT)
set_property(TARGET objlib PROPERTY POSITION_INDEPENDENT_CODE ON)
target_sources(mcfm PRIVATE $<TARGET_OBJECTS:objlib>)
target_include_directories(objlib PRIVATE src/Inc .)
target_include_directories(mcfm PRIVATE src/Inc .)

## MCFM as a library

if(${with_library})
    add_library(libmcfm SHARED)
    set_target_properties(libmcfm PROPERTIES OUTPUT_NAME mcfm)
    target_sources(libmcfm PRIVATE $<TARGET_OBJECTS:objlib>)
    target_include_directories(libmcfm PRIVATE src/Inc)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DWITH_LIBRARY")

    if (DEFINED OLDIR)
      message("Creating test programs using OpenLoops from ${OLDIR}")
      add_executable(oltest)
      find_library(openloops_lib NAMES openloops REQUIRED)
      target_include_directories(oltest PRIVATE src/Inc "${OLDIR}/include")
      add_executable(oltest2)
      target_include_directories(oltest2 PRIVATE src/Inc "${OLDIR}/include")
    endif()
    if (DEFINED RCLDIR)
       message("Creating test program using Recola from ${RCLDIR}")
       add_executable(rcltest)
       find_library(recola_lib NAMES recola REQUIRED)
       target_include_directories(rcltest PRIVATE src/Inc "${RCLDIR}/include")
       add_executable(rcltest2)
       target_include_directories(rcltest2 PRIVATE src/Inc "${RCLDIR}/include")
    endif()
    if (DEFINED RCLHEFTDIR)
       message("Creating test program using Recola for HEFT from ${RCLHEFTDIR}")
       add_executable(rcltest_heft)
       find_library(recola_heft_lib NAMES recola_heft REQUIRED)
       target_include_directories(rcltest_heft PRIVATE src/Inc "${RCLHEFTDIR}/include")
    endif()
    if (DEFINED MLDIR)
       message("Creating test program using MadLoop from ${MLDIR}")
       add_executable(mltest)
       target_include_directories(mltest PRIVATE src/Inc)
       target_compile_definitions(mltest PUBLIC LIB_SUFFIX="${CMAKE_SHARED_LIBRARY_SUFFIX}")
       add_executable(mltest2)
       target_sources(mltest2 PRIVATE $<TARGET_OBJECTS:objlib>)
       target_include_directories(mltest2 PRIVATE src/Inc)
       target_compile_definitions(mltest2 PUBLIC LIB_SUFFIX="${CMAKE_SHARED_LIBRARY_SUFFIX}")
    endif()
endif()

add_subdirectory(src)
add_subdirectory(lib/TensorReduction)
add_subdirectory(lib/SpecialFunctions)
add_subdirectory(lib/AMOS)
add_subdirectory(lib/Chaplin-1.2)

add_dependencies(objlib qd qcdloop liboneloop handyg)

if(${with_vvamp})
    add_compile_definitions(WITH_VVAMP=1)

    ExternalProject_Add(libvvamp
        DOWNLOAD_COMMAND cp -r ${CMAKE_SOURCE_DIR}/lib/VVamp ${CMAKE_BINARY_DIR}
        PREFIX ${CMAKE_BINARY_DIR}/VVamp
        SOURCE_DIR ${CMAKE_BINARY_DIR}/VVamp
        BUILD_IN_SOURCE ON
        PATCH_COMMAND mkdir -p ${CMAKE_BINARY_DIR}/local
        CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER} -DCMAKE_Fortran_FLAGS=-fPIC
        BUILD_COMMAND ${MAKE}
        INSTALL_COMMAND mkdir -p ${CMAKE_BINARY_DIR}/local/include && cp ${CMAKE_BINARY_DIR}/VVamp/mod_vvamp.mod ${CMAKE_BINARY_DIR}/local/include 
    )
    add_library(vvamp STATIC IMPORTED)
    set_property(TARGET vvamp PROPERTY IMPORTED_LOCATION "${CMAKE_BINARY_DIR}/VVamp/libvvamp.a")
    set(linkvvamp vvamp)

    add_dependencies(objlib libvvamp)
else()
    set(linkvvamp "")
endif()

set_target_properties(objlib PROPERTIES
    CXX_STANDARD 11
    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include"
    LINKER_LANGUAGE Fortran)
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

add_library(oneloop STATIC IMPORTED)
set_property(TARGET oneloop PROPERTY IMPORTED_LOCATION "${CMAKE_BINARY_DIR}/lib/oneloop/liboneloop.a")

add_library(qcdloop_lib_static STATIC IMPORTED)
set_property(TARGET qcdloop_lib_static PROPERTY IMPORTED_LOCATION "${CMAKE_BINARY_DIR}/local/lib/libqcdloop.a")
set_property(TARGET qcdloop_lib_static PROPERTY IMPORTED_NO_SONAME TRUE)
add_dependencies(qcdloop_lib_static qcdloop)

add_library(qcdloop_lib_shared SHARED IMPORTED)
set_property(TARGET qcdloop_lib_shared PROPERTY IMPORTED_LOCATION "${CMAKE_BINARY_DIR}/local/lib/libqcdloop.${CMAKE_SHARED_LIBRARY_SUFFIX}")
set_property(TARGET qcdloop_lib_shared PROPERTY IMPORTED_NO_SONAME TRUE)
add_dependencies(qcdloop_lib_shared qcdloop)

add_library(qd_lib_static STATIC IMPORTED)
set_property(TARGET qd_lib_static PROPERTY IMPORTED_LOCATION "${CMAKE_BINARY_DIR}/local/lib/libqd.a")
set_property(TARGET qd_lib_static PROPERTY IMPORTED_NO_SONAME TRUE)
add_dependencies(qd_lib_static qd)

add_library(qd_lib_shared SHARED IMPORTED)
set_property(TARGET qd_lib_shared PROPERTY IMPORTED_LOCATION "${CMAKE_BINARY_DIR}/local/lib/libqd.${CMAKE_SHARED_LIBRARY_SUFFIX}")
set_property(TARGET qd_lib_shared PROPERTY IMPORTED_NO_SONAME TRUE)
add_dependencies(qd_lib_shared qd)

add_library(qdmod_lib_static STATIC IMPORTED)
set_property(TARGET qdmod_lib_static PROPERTY IMPORTED_LOCATION "${CMAKE_BINARY_DIR}/local/lib/libqdmod.a")
set_property(TARGET qdmod_lib_static PROPERTY IMPORTED_NO_SONAME TRUE)
add_dependencies(qdmod_lib_static qd)

add_library(qdmod_lib_shared SHARED IMPORTED)
set_property(TARGET qdmod_lib_shared PROPERTY IMPORTED_LOCATION "${CMAKE_BINARY_DIR}/local/lib/libqdmod.${CMAKE_SHARED_LIBRARY_SUFFIX}")
set_property(TARGET qdmod_lib_shared PROPERTY IMPORTED_NO_SONAME TRUE)
add_dependencies(qdmod_lib_shared qd)

add_library(handyg_lib_static STATIC IMPORTED)
set_property(TARGET handyg_lib_static PROPERTY IMPORTED_LOCATION "${CMAKE_BINARY_DIR}/local/lib/libhandyg.a")
set_property(TARGET handyg_lib_static PROPERTY IMPORTED_NO_SONAME TRUE)
add_dependencies(handyg_lib_static handyg)

add_dependencies(objlib qdmod_lib_static)
add_dependencies(objlib qd_lib_static)
add_dependencies(objlib qcdloop_lib_static)
add_dependencies(objlib handyg_lib_static)

if(use_internal_lhapdf)
    ExternalProject_Add(lhapdf
        URL ${CMAKE_SOURCE_DIR}/lib/LHAPDF-6.5.1.tar.gz
        SOURCE_DIR ${CMAKE_BINARY_DIR}/LHAPDF-6.5.1
        PATCH_COMMAND mkdir -p ${CMAKE_BINARY_DIR}/local
        BUILD_IN_SOURCE ON
        CONFIGURE_COMMAND ./configure --with-pic --disable-lhaglue --disable-lhaglue-cxx --disable-python --prefix=${CMAKE_BINARY_DIR}/local CC=${CMAKE_C_COMPILER} CXX=${CMAKE_CXX_COMPILER}
        BUILD_COMMAND ${MAKE}
        INSTALL_COMMAND ${CMAKE_MAKE_PROGRAM} install
    )
    add_library(lhapdf_lib_static STATIC IMPORTED)
    set_property(TARGET lhapdf_lib_static PROPERTY IMPORTED_LOCATION "${CMAKE_BINARY_DIR}/local/lib/libLHAPDF.a")
    set_property(TARGET lhapdf_lib_static PROPERTY IMPORTED_NO_SONAME TRUE)

    add_library(lhapdf_lib STATIC IMPORTED)
    set_property(TARGET lhapdf_lib PROPERTY IMPORTED_LOCATION "${CMAKE_BINARY_DIR}/local/lib/libLHAPDF.${CMAKE_SHARED_LIBRARY_SUFFIX}")
    set_property(TARGET lhapdf_lib PROPERTY IMPORTED_NO_SONAME TRUE)

    add_dependencies(lhapdf_lib_static lhapdf)
    target_link_libraries(mcfm lhapdf_lib_static)
    target_include_directories(objlib PRIVATE "${CMAKE_BINARY_DIR}/local/include" "${CMAKE_BINARY_DIR}/local/include/qd")
    add_dependencies(objlib lhapdf)
elseif(use_external_lhapdf)
    find_library(lhapdf_lib NAMES LHAPDF REQUIRED)
    target_link_libraries(mcfm ${lhapdf_lib})
    target_include_directories(objlib PRIVATE "${CMAKE_BINARY_DIR}/local/include" "${CMAKE_BINARY_DIR}/local/include/qd")
    if (EXISTS "${lhapdf_include_path}")
        target_include_directories(objlib PRIVATE "${lhapdf_include_path}")
    endif()
endif()

## MCFM as a library

if(${with_library})
    if(use_internal_lhapdf)
        target_link_libraries(libmcfm qdmod_lib_static qd_lib_static qcdloop_lib_static oneloop handyg_lib_static ${linkvvamp} quadmath stdc++ lhapdf_lib_static)
    else()
        target_link_libraries(libmcfm qdmod_lib_static qd_lib_static qcdloop_lib_static oneloop handyg_lib_static ${linkvvamp} quadmath stdc++ ${lhapdf_lib})
    endif()
    if (DEFINED OLDIR)
      target_link_libraries(oltest libmcfm ${openloops_lib})
      target_link_libraries(oltest2 libmcfm ${openloops_lib})
    endif()
    if (DEFINED RCLDIR)
       target_link_libraries(rcltest libmcfm ${recola_lib})
       target_link_libraries(rcltest2 libmcfm ${recola_lib})
    endif()
    if (DEFINED RCLHEFTDIR)
       target_link_libraries(rcltest_heft libmcfm ${recola_heft_lib})
    endif()
    if (DEFINED MLDIR)
       target_link_libraries(mltest libmcfm)
       target_link_libraries(mltest2 libmcfm)
    endif()
endif()

target_link_libraries(mcfm qdmod_lib_static qd_lib_static qcdloop_lib_static oneloop handyg_lib_static ${linkvvamp} quadmath stdc++)

set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
set(CMAKE_INSTALL_RPATH "${CMAKE_BINARY_DIR}/local/lib")

## Installation

install(FILES "${CMAKE_SOURCE_DIR}/Bin/process.DAT" DESTINATION "${CMAKE_BINARY_DIR}")
install(FILES "${CMAKE_SOURCE_DIR}/Bin/br.sm1" DESTINATION "${CMAKE_BINARY_DIR}")
install(FILES "${CMAKE_SOURCE_DIR}/Bin/br.sm2" DESTINATION "${CMAKE_BINARY_DIR}")
install(DIRECTORY "${CMAKE_SOURCE_DIR}/Bin/PDFs" DESTINATION "${CMAKE_BINARY_DIR}")

if(${with_library})
    if ((DEFINED OLDIR) OR (DEFINED RCLDIR) OR (DEFINED MLDIR))
        install(FILES "${CMAKE_SOURCE_DIR}/src/BLHA/params.lh" DESTINATION "${CMAKE_BINARY_DIR}")
        message("Please consult the manual for using the C++ interface test programs.")
    endif()

    if (DEFINED OLDIR)
        install(FILES "${CMAKE_SOURCE_DIR}/src/BLHA/oltest.sh" DESTINATION "${CMAKE_BINARY_DIR}"
            PERMISSIONS OWNER_READ GROUP_READ WORLD_READ OWNER_EXECUTE GROUP_EXECUTE WORLD_EXECUTE)
    endif()
    if (DEFINED RCLDIR OR DEFINED RCLHEFTDIR)
        install(FILES "${CMAKE_SOURCE_DIR}/src/BLHA/rcltest.sh" DESTINATION "${CMAKE_BINARY_DIR}"
            PERMISSIONS OWNER_READ GROUP_READ WORLD_READ OWNER_EXECUTE GROUP_EXECUTE WORLD_EXECUTE)
    endif()
    if (DEFINED MLDIR)
        install(FILES "${CMAKE_SOURCE_DIR}/src/BLHA/mltest.sh" DESTINATION "${CMAKE_BINARY_DIR}"
            PERMISSIONS OWNER_READ GROUP_READ WORLD_READ OWNER_EXECUTE GROUP_EXECUTE WORLD_EXECUTE)
    endif()
endif()
