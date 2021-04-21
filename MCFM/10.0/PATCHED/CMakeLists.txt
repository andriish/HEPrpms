cmake_minimum_required (VERSION 3.11)
enable_language(Fortran)
project (mcfm C CXX Fortran)
SET(CMAKE_CASE_INSENSITIVE_TARGETS ON)
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    #target_compile_options( mcfm PRIVATE -Wall -Wextra -Wunreachable-code -Wpedantic)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp -finit-local-zero -Wall -ffixed-line-length-none -ffree-line-length-512 -Wno-unused-variable -Wno-unused-function -Wno-unused-dummy-argument -Wno-conversion -Wno-surprising -Wno-c-binding-type -Wno-real-q-constant -fopenmp")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpp -qopenmp -stand f08 -traceback -132 -diag-disable 1622,5268,6916,6919,7416,7343,7373,7346,7713,1789,7355,7356,7026,6243")
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

add_library(mcfm STATIC)
add_executable(mcfmmain src/Need/mcfmmain.f)


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
target_include_directories(mcfmmain PRIVATE src/Inc .)

add_subdirectory(src)
add_subdirectory(TensorReduction)


set_target_properties(mcfm PROPERTIES 
    CXX_STANDARD 11
    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include"
    LINKER_LANGUAGE Fortran)

set_target_properties(mcfmmain PROPERTIES 
    CXX_STANDARD 11
    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include"
    LINKER_LANGUAGE Fortran)

if(use_mpi)
    if(${CMAKE_VERSION} VERSION_LESS "3.12.0") 
      add_definitions(-DHAVE_MPI=1)
    else()
      add_compile_definitions(HAVE_MPI=1)    
    endif()
    find_package(MPI REQUIRED)
    target_link_libraries(mcfm ${MPI_Fortran_LIBRARIES})
    message("Using MPI!")
endif(use_mpi)

if (use_coarray)
    if(${CMAKE_VERSION} VERSION_LESS "3.12.0") 
      add_definitions(-DHAVE_COARRAY=1)
    else()
      add_compile_definitions(HAVE_COARRAY=1) 
    endif()
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
    if(${CMAKE_VERSION} VERSION_LESS "3.12.0") 
      add_definitions(-DHAVE_LHAPDF=1)
    else()
      add_compile_definitions(HAVE_LHAPDF=1)
    endif()
elseif(use_external_lhapdf)
    find_library(lhapdf_lib NAMES LHAPDF LHAPDF-6.3.0 REQUIRED)
    target_link_libraries(mcfm ${lhapdf_lib})
    if(${CMAKE_VERSION} VERSION_LESS "3.12.0") 
      add_definitions(-DHAVE_LHAPDF=1)
    else()
      add_compile_definitions(HAVE_LHAPDF=1)
    endif()
endif()

target_link_libraries(mcfmmain mcfm qdmod_lib qd_lib qcdloop_lib quadmath stdc++)
set_target_properties(mcfmmain PROPERTIES OUTPUT_NAME "mcfm")
set(THE_LIB ${CMAKE_BINARY_DIR}/libmcfm.a)

add_custom_target(combined
        COMMAND ar -x $<TARGET_FILE:qcdloop_lib>
        COMMAND ar -x $<TARGET_FILE:qd_lib>
        COMMAND ar -x $<TARGET_FILE:qdmod_lib>
        COMMAND ar -x $<TARGET_FILE:mcfm>
        COMMAND ar -qcs ${THE_LIB} *.o
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
        DEPENDS mcfm qdmod_lib qd_lib qcdloop_lib
        )
include(GNUInstallDirs)
install(FILES ${THE_LIB} DESTINATION ${CMAKE_INSTALL_LIBDIR} RENAME libMCFM.a)
install(TARGETS mcfmmain DESTINATION ${CMAKE_INSTALL_BINDIR})

