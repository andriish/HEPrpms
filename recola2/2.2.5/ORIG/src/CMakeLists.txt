# File: CMakeLists.txt
# Author: Jean-Nicolas Lang
# Description: CMake configure file for Recola2 sources
# Last Modified: September 30, 2024

option (static "shared_libraries" OFF)
option (lib_suffix "Whether to add suffix to library" "")

# FFLAGS depend on the compiler
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

set (unoptimized "-O0")
if (Fortran_COMPILER_NAME MATCHES "gfortran.*" OR Fortran_COMPILER_NAME MATCHES "f95")
  # gfortran
  set (debug " -g -ffixed-line-length-132")
  set (checks " -fcheck=all -ffpe-trap=invalid,zero,overflow")
  set (inits " -finit-integer=-1000000 -finit-logical=true -finit-real=nan")
  set (warnings " -Wall ")
  set (maths " -fno-fast-math -fno-omit-frame-pointer -ftrapv")
  set (debug "${debug} ${checks} ${inits} ${maths} ${warnings}")
  set (CMAKE_Fortran_FLAGS_DEBUG "${debug}")
  set (CMAKE_Fortran_FLAGS_RELEASE "-ffixed-line-length-132")
  include(CheckFortranCompilerFlag)
  check_fortran_compiler_flag("-Ofast" _ofast_flag)
  if(_ofast_flag)
    set (optimized "-Ofast")
  else()
    set (optimized "-O2")
  endif()
  if (NOT static)
    #check_fortran_compiler_flag("-flto" _lto_flag)
    #if(_lto_flag)
    #  set(optimized "${optimized} -flto")
    #endif()
  endif()
  add_definitions(-DGNU_NAME_MANGLING)
elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
  set (basic " -132")
  set (opt_DEBUG " -g -traceback -O0 -check all")
  set (opt_DEBUG "${opt_DEBUG} -warn all")
  set (CMAKE_Fortran_FLAGS_RELEASE "${basic} ${opt_RELEASE}")
  set (CMAKE_Fortran_FLAGS_DEBUG "${basic} ${opt_DEBUG}")
  set (optimized "-fast -ipo")
  add_definitions(-DINTEL_NAME_MANGLING)
elseif (Fortran_COMPILER_NAME MATCHES "pgf77")
  # g77 (untested)
  set (basic "${basic} -ffixed-line-length-132  -g77libs -Msecond_underscore")
  set (opt_DEBUG "-O0 -Mbounds -frange-check -g -fcheck=all")
  set (opt_DEBUG "${opt_DEBUG} -Wall -Wtabs -Wextra -Wno-unused")
  set (opt_DEBUG "${opt_DEBUG} -Wno-unused-dummy-argument")
  set (opt_DEBUG "${opt_DEBUG} -Wno-unused-parameter")
  set (CMAKE_Fortran_FLAGS_RELEASE "${basic} ${opt_RELEASE}")
  set (CMAKE_Fortran_FLAGS_DEBUG "${basic} ${opt_DEBUG}")
  set (optimized "-O2 -fast")
else (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  set (CMAKE_Fortran_FLAGS_DEBUG   "-g")
  set (optimized "-O2")
endif ()

set(CMAKE_Fortran_FLAGS_DEBUG   "${CMAKE_Fortran_FLAGS_DEBUG} -cpp")
set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -cpp")


set(RECOLA_FILES "")
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_SOURCE_PATH}/recola.f90)
set(RECOLA_CORE_PATH ${RECOLA_SOURCE_PATH}/core)
set(RECOLA_UTIL_PATH ${RECOLA_SOURCE_PATH}/util)
set(RECOLA_API_PATH ${RECOLA_SOURCE_PATH}/api)

if(EXISTS ${RECOLA_ROOT_PATH}/.git)
  find_package(Git)
  if(GIT_FOUND)
    # working branch
    execute_process(
      COMMAND git rev-parse --abbrev-ref HEAD
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      OUTPUT_VARIABLE GIT_BRANCH
      OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    # abbreviated commit hash
    execute_process(
      COMMAND git log -1 --format=%h
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      OUTPUT_VARIABLE GIT_COMMIT_HASH
      OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    MESSAGE( STATUS "Git branch:  ${GIT_BRANCH}" )
    MESSAGE( STATUS "Git version: ${GIT_COMMIT_HASH}" )
  else(GIT_FOUND)
    SET(GIT_BRANCH "Unknown")
    SET(GIT_COMMIT_HASH 0)
  endif(GIT_FOUND)
endif(EXISTS ${RECOLA_ROOT_PATH}/.git)

CONFIGURE_FILE(${RECOLA_CORE_PATH}/globals_rcl.f90.in ${RECOLA_CORE_PATH}/globals_rcl.f90 @ONLY)

#========#
#  Core  #
#========#

set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_CORE_PATH}/amplitude_rcl.f90)
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_CORE_PATH}/colourflow_rcl.f90)
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_CORE_PATH}/currents_rcl.f90)
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_CORE_PATH}/reset_rcl.f90)
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_CORE_PATH}/globals_rcl.f90)
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_CORE_PATH}/skeleton_rcl.f90)
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_CORE_PATH}/tables_rcl.f90)
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_CORE_PATH}/wave_functions_rcl.f90)
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_CORE_PATH}/tree_vertices_rcl.f90)
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_CORE_PATH}/loop_vertices_rcl.f90)
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_CORE_PATH}/order_rcl.f90)

#========#
#  Util  #
#========#

set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_UTIL_PATH}/draw_current_rcl.f90)
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_UTIL_PATH}/form_rcl.f90)
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_UTIL_PATH}/memory_logger_rcl.f90)
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_UTIL_PATH}/random_psp_rcl.f90)
find_path(rambo_path
          NAMES rambo.f90
          PATHS ${RECOLA_UTIL_PATH})
find_path(ranlux_path
          NAMES ranlux.f90
          PATHS ${RECOLA_UTIL_PATH})
if(rambo_path AND ranlux_path)
  message(STATUS "Including rambo in the build.")
  set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_UTIL_PATH}/rambo.f90)
  set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_UTIL_PATH}/ranlux.f90)
  add_definitions(-DWITHRAMBO)
else()
  remove_definitions(-DWITHRAMBO)
endif()
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_UTIL_PATH}/statistics_rcl.f90)
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_UTIL_PATH}/wrapper_rcl.f90)
if(with_smtests)
  set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_UTIL_PATH}/check_rcl.f90)
endif()

#=======#
#  API  #
#=======#

set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_API_PATH}/input_rcl.f90)
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_API_PATH}/process_computation_rcl.f90)
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_API_PATH}/process_definition_rcl.f90)
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_API_PATH}/process_generation_rcl.f90)
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_API_PATH}/collier_interface_rcl.f90)
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_API_PATH}/recola1_interface_rcl.f90)
set(RECOLA_FILES ${RECOLA_FILES} ${RECOLA_API_PATH}/extended_higgs_interface_rcl.f90)

string(TOLOWER "${CMAKE_BUILD_TYPE}" CBT)
if (CBT MATCHES debug)
  set(OFLAG ${unoptimized})
else()
  set(OFLAG ${optimized})
endif ()
set_source_files_properties(${RECOLA_FILES} PROPERTIES COMPILE_FLAGS ${OFLAG})


# gather all source files
set(SOURCE ${RECOLA_FILES})

# define the recola library
if (static)
  message(STATUS "Building recola2 as static library")
  if (lib_suffix)
    set(RECOLA_LIBRARY "recola_${lib_suffix}")
  else()
    set(RECOLA_LIBRARY "recola")
  endif()
  add_library (${RECOLA_LIBRARY} STATIC ${SOURCE})
else()
  message(STATUS "Building recola2 as shared library")
  if (lib_suffix)
    set(RECOLA_LIBRARY "recola_${lib_suffix}")
  else()
    set(RECOLA_LIBRARY "recola")
  endif()
  add_library (${RECOLA_LIBRARY} SHARED ${SOURCE})
endif()
set(RECOLA_LIBRARY ${RECOLA_LIBRARY} PARENT_SCOPE)

#-------------------#
#  Include Collier  #
#-------------------#

function(reset_collier_cache)
  unset(COLLIER_INCLUDE_DIR CACHE)
  unset(COLLIER_LIBRARY_DIR CACHE)
  unset(COLLIER_LIBRARY_PATH CACHE)
  unset(COLLIER_SYSCONFIG_DIR CACHE)
  unset(collier_DIR CACHE)
endfunction()

# Option 1: collier_path given
if (collier_path)
  unset(COLLIER_SEARCH_PATH CACHE)
  set (COLLIER_SEARCH_PATH ${collier_path} CACHE PATH "Cached path to collier library")
  unset(collier_path CACHE)
  reset_collier_cache()
# Option 2: via environment variable COLLIER_PATH
elseif(DEFINED ENV{COLLIER_PATH})
  unset(COLLIER_SEARCH_PATH CACHE)
  set (COLLIER_SEARCH_PATH $ENV{COLLIER_PATH} CACHE PATH "Cached path to collier library")
  reset_collier_cache()
endif()


find_package(collier ${COLLIER_MIN_VERSION} REQUIRED HINTS ${COLLIER_SEARCH_PATH} NO_DEFAULT_PATH)
set(COLLIER_INCLUDE_DIR ${COLLIER_INCLUDE_DIR} PARENT_SCOPE)
message(STATUS "Collier library path: ${COLLIER_LIBRARY_PATH}")
message(STATUS "Collier include path: ${COLLIER_INCLUDE_DIR}")
message(STATUS "Collier version exact: ${collier_VERSION}")
message(STATUS "RECOLA LIBRARY : ${RECOLA_LIBRARY}")
target_link_libraries (${RECOLA_LIBRARY} ${COLLIER_LIBRARY_PATH})

#----------------------#
#  Include model file  #
#----------------------#

function(reset_modelfile_cache)
  unset(MODELFILE_INCLUDE_DIR CACHE)
  unset(modelfile_library_dir CACHE)
  unset(MODELFILE_LIBRARY_PATH CACHE)
  unset(MODELFILE_SYSCONFIG_DIR CACHE)
  unset(modelfile_DIR CACHE)
endfunction()

# Option 1: modelfile_path given
if (modelfile_path)
  unset(MDL_SEARCH_PATH CACHE)
  set (MDL_SEARCH_PATH ${modelfile_path} CACHE PATH "Cached path to model file library")
  unset(modelfile_path CACHE)
  reset_modelfile_cache()
# Option 2: via environment variable MODELFILE_PATH
elseif(DEFINED ENV{MODELFILE_PATH})
  unset(MDL_SEARCH_PATH CACHE)
  set (MDL_SEARCH_PATH $ENV{MODELFILE_PATH} CACHE PATH "Cached path to model file library")
  reset_modelfile_cache()
endif()
unset(MODELFILE_INCLUDE_DIR CACHE)
unset(MODELFILE_LIBRARY_DIR CACHE)
unset(MODELFILE_LIBRARY_PATH CACHE)
unset(MODELFILE_SYSCONFIG_DIR CACHE)
unset(modelfile_DIR CACHE)

find_package(modelfile ${MODELFILE_MIN_VERSION} REQUIRED HINTS ${MDL_SEARCH_PATH} NO_DEFAULT_PATH)
set(MODELFILE_INCLUDE_DIR ${MODELFILE_INCLUDE_DIR} PARENT_SCOPE)
message(STATUS "Model file library path: ${MODELFILE_LIBRARY_PATH}")
message(STATUS "Model file include path: ${MODELFILE_INCLUDE_DIR}")
message(STATUS "Model file version exact: ${modelfile_VERSION}")
target_link_libraries (${RECOLA_LIBRARY} ${MODELFILE_LIBRARY_PATH})

###########
#  otter  #
###########

function(reset_otter_cache)
  unset(OTTER_INCLUDE_DIR CACHE)
  unset(OTTER_LIBRARY_DIR CACHE)
  unset(OTTER_LIBRARY_PATH CACHE)
  unset(OTTER_SYSCONFIG_DIR CACHE)
  unset(otter_DIR CACHE)
endfunction()

# Option 1: otter_path given
if (otter_path)
  unset(OTTER_SEARCH_PATH CACHE)
  set (OTTER_SEARCH_PATH ${otter_path} CACHE PATH "Cached path to otter library")
  unset(otter_path CACHE)
  reset_otter_cache()
# Option 2: via environment variable otter_path
elseif(DEFINED ENV{otter_path})
  unset(OTTER_SEARCH_PATH CACHE)
  set (OTTER_SEARCH_PATH $ENV{otter_path} CACHE PATH "Cached path to otter library")
  reset_otter_cache()
endif()

find_package(otter 0.9.2 HINTS ${OTTER_SEARCH_PATH} NO_DEFAULT_PATH)
if(otter_FOUND)
  set(OTTER_INCLUDE_DIR ${OTTER_INCLUDE_DIR} PARENT_SCOPE)
  message(STATUS "Otter library path: ${OTTER_LIBRARY_PATH}")
  message(STATUS "Otter include path: ${OTTER_INCLUDE_DIR}")
  message(STATUS "Otter version exact: ${otter_VERSION}")
  target_link_libraries (recola ${OTTER_LIBRARY_PATH})
  add_definitions(-DWITHOTTER)
else()
  remove_definitions(-DWITHOTTER)
endif()
set(otter_FOUND ${otter_FOUND} PARENT_SCOPE)

#============#
#  pyrecola  #
#============#

unset(PYTHONLIBS_FOUND CACHE)
unset(PYTHONINTERP_FOUND CACHE)
unset(PYTHON_EXECUTABLE CACHE)
unset(PYTHON_INCLUDE_DIR CACHE)
unset(PYTHON_LIBRARY CACHE)
unset(PYTHON_LIBRARY_DEBUG CACHE)
remove_definitions(-DPYTHON3)
if (NOT static)
  if(with_python3)
    set(Python_ADDITIONAL_VERSIONS 3.4 3.5 3.6 3.8)
    find_package(Python3 COMPONENTS Interpreter Development)
    add_definitions(-DPYTHON3)
  endif()

  if(NOT PYTHON3_FOUND)
    message(STATUS "Python3 not found! Pyrecola is not built.")
  else()
    message(STATUS "Pyrecola will be built.")
  endif()

  if(PYTHON3_FOUND)
    set(PYRECOLA_FILES "")
    set(PYRECOLA_FILES ${PYRECOLA_SOURCE_PATH}/pyrecola.c)
    add_library(pyrecola MODULE ${PYRECOLA_FILES})
    include_directories(${Python3_INCLUDE_DIRS})
    target_link_libraries(pyrecola ${Python3_LIBRARIES} recola)
    set_target_properties(pyrecola PROPERTIES PREFIX "")
  endif()
else()
  message(WARNING "Static compilation. Pyrecola is not built.")
endif()

#-------------------------------------#
#  Recola installation configuration  #
#-------------------------------------#

set(RECOLA_HEADERS "")
set(RECOLA_HEADERS ${RECOLA_HEADERS} "${CMAKE_Fortran_MODULE_DIRECTORY}/recola.mod")
set(RECOLA_HEADERS ${RECOLA_HEADERS} "${RECOLA_INCLUDE_PATH}/recola.h")
set(RECOLA_HEADERS ${RECOLA_HEADERS} "${RECOLA_INCLUDE_PATH}/recola.hpp")
set_target_properties(${RECOLA_LIBRARY} PROPERTIES PUBLIC_HEADER "${RECOLA_HEADERS}")

message("")
if (CBT MATCHES debug)
  message("Build type: Debug")
else()
  message("Build type: Release")
endif ()

message ("Recola:  Fortran compiler       = ${Fortran_COMPILER_NAME}")
message ("         Fortran compiler ID    = ${CMAKE_Fortran_COMPILER_ID}")
if (CBT MATCHES "debug")
  message ("         Fortran flags          = ${CMAKE_Fortran_FLAGS_DEBUG}")
else ()
  set(CMAKE_Fortran_FLAGS ${CMAKE_Fortran_FLAGS_RELEASE})
  message ("         Fortran flags          = ${CMAKE_Fortran_FLAGS_RELEASE}")
endif ()
message ("         Optimization flag      = ${OFLAG}")
message ("         Library install prefix = ${LIB_INSTALL_DIR}")
message ("         Include install prefix = ${INCLUDE_INSTALL_DIR}\n")

if(PYTHON3_FOUND)
  string(REPLACE "." ";" PY_VERSION_LIST ${Python3_VERSION})
  list(GET PY_VERSION_LIST 0 PY_VERSION_MAJOR)
  list(GET PY_VERSION_LIST 1 PY_VERSION_MINOR)
  list(GET PY_VERSION_LIST 2 PY_VERSION_PATCH)

  unset(PYTHON_SITE_DIR CACHE)
  set(PYTHON_SITE_DIR
    "lib/python${PY_VERSION_MAJOR}.${PY_VERSION_MINOR}/site-packages" CACHE PATH
    "Relative installation path for python library")
  if(NOT IS_ABSOLUTE "${PYTHON_SITE_DIR}")
    set(PYTHON_SITE_DIR "${CMAKE_INSTALL_PREFIX}/${PYTHON_SITE_DIR}")
  endif()
  message("Pyrecola: Library install prefix = ${PYTHON_SITE_DIR}\n")
  install(TARGETS pyrecola
          LIBRARY DESTINATION ${PYTHON_SITE_DIR}
          )
endif()

include(PackageConfigInstall)
if (static)
  PACKAGE_CONFIG_INSTALL(${RECOLA_LIBRARY} ${RECOLA_VERSION} STATIC)
else()
  PACKAGE_CONFIG_INSTALL(${RECOLA_LIBRARY} ${RECOLA_VERSION} SHARED)
endif()
