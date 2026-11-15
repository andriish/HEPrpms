# - Try to find OPENLOOPS
# Defines:
#
#  OPENLOOPS_FOUND
#  OPENLOOPS_INCLUDE_DIR
#  OPENLOOPS_INCLUDE_DIRS (not cached)
#  OPENLOOPS_LIBRARY
#  OPENLOOPS_LIBRARIES (not cached)
#  OPENLOOPS_LIBRARY_DIR (not cached)

if (OPENLOOPS_ROOT_DIR OR OPENLOOPS_DIR OR (DEFINED ENV{OPENLOOPS_ROOT_DIR}) OR (DEFINED ENV{OPENLOOPS_DIR}) )
  set(OPENLOOPS_SEARCH_DIRS "" CACHE STRING "" FORCE)
  if (OPENLOOPS_ROOT_DIR)
    list (APPEND OPENLOOPS_SEARCH_DIRS "${OPENLOOPS_ROOT_DIR}" )
  endif()
  if (OPENLOOPS_DIR)
    list (APPEND OPENLOOPS_SEARCH_DIRS "${OPENLOOPS_DIR}" )
  endif()
  if (DEFINED ENV{OPENLOOPS_ROOT_DIR})
    list (APPEND OPENLOOPS_SEARCH_DIRS "$ENV{OPENLOOPS_ROOT_DIR}" )
  endif()
  if (DEFINED ENV{OPENLOOPS_DIR})
    list (APPEND OPENLOOPS_SEARCH_DIRS "$ENV{OPENLOOPS_DIR}" )
  endif()
endif()
set(OPENLOOPS_VERSION 0.0.0)

if (OPENLOOPS_SEARCH_DIRS)
  find_path(OPENLOOPS_PREFIX proclib/channels_public.rinfo PATHS ${OPENLOOPS_SEARCH_DIRS} PATH_SUFFIXES . lib/openloops lib64/openloops  NO_DEFAULT_PATH)
  find_library(OPENLOOPS_LIBRARY NAMES openloops PATHS ${OPENLOOPS_SEARCH_DIRS}  PATH_SUFFIXES . lib lib64 lib/openloops/lib lib64/openloops/lib NO_DEFAULT_PATH)
else()
  find_path(OPENLOOPS_PREFIX proclib/channels_public.rinfo PATH_SUFFIXES . lib/openloops lib64/openloops )
  find_library(OPENLOOPS_LIBRARY NAMES openloops PATH_SUFFIXES .  lib lib64  lib/openloops/lib lib64/openloops/lib)
endif()
find_path(OPENLOOPS_INCLUDE_DIR openloops.mod PATH_SUFFIXES include/openloops/lib_src/openloops/mod/)
if ( NOT OPENLOOPS_INCLUDE_DIR)
  set(OPENLOOPS_INCLUDE_DIR "${OPENLOOPS_PREFIX}")
endif()
if (EXISTS "${OPENLOOPS_PREFIX}/pyol/config/default.cfg")
  file(READ ${OPENLOOPS_PREFIX}/pyol/config/default.cfg defaultcfgstr)
  string(REGEX MATCH "^release.*"  defaultcfgstr2  "${defaultcfgstr}")
  string(REGEX REPLACE ".*release = ([0-9]+\\.[0-9]+\\.[0-9]+).*" "\\1"  OPENLOOPS_VERSION "${defaultcfgstr2}")
  if (NOT OPENLOOPS_VERSION)
    set(OPENLOOPS_VERSION 0.0.0)
  endif()  
endif()
mark_as_advanced(OPENLOOPS_INCLUDE_DIR OPENLOOPS_LIBRARY OPENLOOPS_PREFIX)
include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(OpenLoops REQUIRED_VARS  OPENLOOPS_LIBRARY OPENLOOPS_INCLUDE_DIR OPENLOOPS_PREFIX
                                  VERSION_VAR OPENLOOPS_VERSION
                                  )
set(OPENLOOPS_LIBRARIES ${OPENLOOPS_LIBRARY})
get_filename_component(OPENLOOPS_LIBRARY_DIR ${OPENLOOPS_LIBRARY} PATH)

if(OPENLOOPS_FOUND AND NOT TARGET OpenLoops::OpenLoops)
    add_library( OpenLoops::OpenLoops SHARED IMPORTED)
    set_target_properties( OpenLoops::OpenLoops PROPERTIES
        IMPORTED_LOCATION "${OPENLOOPS_LIBRARY}"
        INTERFACE_INCLUDE_DIRECTORIES "${OPENLOOPS_INCLUDE_DIR}"
        IMPORTED_NO_SONAME TRUE
    )
endif()
mark_as_advanced(OpenLoops_FOUND)
