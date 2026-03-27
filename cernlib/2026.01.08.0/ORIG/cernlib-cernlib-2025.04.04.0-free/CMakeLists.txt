cmake_minimum_required(VERSION 3.3.0 FATAL_ERROR)
###########################Options handeling#####################################
###########################Options#####################################
option(CERNLIB_USE_INTERNAL_XBAE          "Forces using internal XBAE." ON)
option(CERNLIB_USE_INTERNAL_LAPACK        "Forces using internal LAPACK/BLAS." ON)
option(CERNLIB_ENABLE_TEST                "Enables building of tests." ON)
option(CERNLIB_ENABLE_MEMORYTEST          "Adds memory check tests." OFF)
option(CERNLIB_FORCE_FLAGS                "Use compilation flags from the command line only." OFF)
option(CERNLIB_POSITION_INDEPENDENT_CODE  "Position independent code for static libraries." OFF)
option(CERNLIB_BUILD_SHARED               "Build shared libraries." OFF)
option(CERNLIB_SHIFT                      "Build with shift support." OFF)
option(CERNLIB_XROOTD                     "Build with xrootd support." OFF)
option(CERNLIB_GFLUKA                     "Build with GFluka." OFF)
option(CERNLIB_FORCE_32                   "Force build of 32 bit libraries." OFF)
option(CERNLIB_QPRINT                     "Debug print statements." OFF)
option(CERNLIB_QDEBUG                     "More Debug print statements." OFF)
option(CERNLIB_QDEVZE                     "Zebra developement." OFF)
option(CERNLIB_NO_SUFFIX                  "Do not append suffixes to binaries." ON)
set(CERNLIB_BUILD_STATIC ON)  #This is always ON, but it is convenient to have it like this from the point of view of logic.
###########################Package@#####################################
project(CERNLIB LANGUAGES C Fortran )
#The complicated way to set the version is used to assure the simple
#replacement of version variable with sed will not break the CMakeLists.txt
if (NOT __CL_VERSION__)
  set(EXTERNAL_VERSION_STRING "2025.02.25")
else()
  set(EXTERNAL_VERSION_STRING "__CL_VERSION__.1")
endif()
string(REPLACE "."  ";" EXTERNAL_VERSION_LIST "${EXTERNAL_VERSION_STRING}")
list(GET EXTERNAL_VERSION_LIST 0 CERNLIB_VERSION_MAJOR)
list(GET EXTERNAL_VERSION_LIST 1 CERNLIB_VERSION_MINOR)
list(GET EXTERNAL_VERSION_LIST 2 CERNLIB_VERSION_PATCH)
set(PROJECT_VERSION ${CERNLIB_VERSION_MAJOR}.${CERNLIB_VERSION_MINOR}.${CERNLIB_VERSION_PATCH})
SET(CERNLIB_VERSION ${CERNLIB_VERSION_MAJOR}.${CERNLIB_VERSION_MINOR})
message(STATUS "CERNLIB: CERNLIB_VERSION=${CERNLIB_VERSION}")
message(STATUS "CERNLIB: CERNLIB_VERSION_MAJOR=${CERNLIB_VERSION_MAJOR}")
message(STATUS "CERNLIB: CERNLIB_VERSION_MINOR=${CERNLIB_VERSION_MINOR}")
message(STATUS "CERNLIB: CERNLIB_VERSION_PATCH=${CERNLIB_VERSION_PATCH}")
message(STATUS "CERNLIB: Building on CMAKE_SYSTEM_NAME=${CMAKE_SYSTEM_NAME}")
if (CMAKE_SYSTEM_NAME MATCHES "Linux")
 set(LINUX 1)
endif()
if (CMAKE_SYSTEM_NAME MATCHES "SunOS")
 set(SunOS 1)
endif()
if(CMAKE_BUILD_TYPE)
  message(STATUS "CERNLIB: Build type: ${CMAKE_BUILD_TYPE}")
else(CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "DEBUG")
  message(STATUS "CERNLIB: Build type not specified, use: ${CMAKE_BUILD_TYPE}")
endif(CMAKE_BUILD_TYPE)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules ${CMAKE_MODULE_PATH})

if (CERNLIB_XROOTD)
  find_package(XRootD REQUIRED)
  Message(STATUS "CERNLIB: XROOTD version:   ${XROOTD_VERSION}")
  Message(STATUS "CERNLIB: XROOTD Headers:   ${XROOTD_INCLUDE_DIR}")
  Message(STATUS "CERNLIB: XROOTD Libraries: ${XROOTD_LIB_DIR}")
  add_compile_options(-D_LARGEFILE_SOURCE -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64)
  include_directories(${XROOTD_INCLUDE_DIR})
  link_directories(${XROOTD_LIB_DIR})
  set(libxrdposix XrdPosixPreload XrdPosix)
  if (${XROOTD_VERSION} VERSION_LESS 5.8.0)
    if (APPLE)
        Message(FATAL_ERROR "CERNLIB: On APPLE you need XROOTD 5.8.0 or newer to make this work.")
      else()
        add_compile_options(-DXROOTD_BUILDIN_HEADERS)
        Message(STATUS "CERNLIB: Using build-in XrdPosix header files")
      endif()
    else()
      Message(STATUS "CERNLIB: Using official XrdPosix header files")
  endif()
endif()

set(CMAKE_VERBOSE_MAKEFILE FALSE)
set(CMAKE_POSITION_INDEPENDENT_CODE FALSE)
if (CERNLIB_QPRINT)
  add_definitions(-DCERNLIB_QPRINT)
endif()
if (CERNLIB_QDEBUG)
  add_definitions(-DCERNLIB_QDEBUG)
endif()
if (CERNLIB_QDEVZE)
  add_definitions(-DCERNLIB_QDEVZE)
endif()
if (CERNLIB_FORCE_32)
  add_definitions(-DCERNLIB_32BITCROSS)
  list(PREPEND CMAKE_SYSTEM_LIBRARY_PATH "/usr/lib")
  message(STATUS "${CMAKE_SYSTEM_LIBRARY_PATH}")
  set( FIND_LIBRARY_USE_LIB32_PATHS  ON)
  set( FIND_LIBRARY_USE_LIB64_PATHS  OFF)
  set(CMAKE_SIZEOF_VOID_P 4)
  set (CMAKE_EXE_LINKER_FLAGS  "-m32 ${CMAKE_EXE_LINKER_FLAGS}")
  #To prevent eraling the suffix in case it is 32
  if (LIB_SUFFIX)
    if ( ${LIB_SUFFIX}  STREQUAL "64" )
      set (LIB_SUFFIX  )
    endif()
  endif()
endif()
if (CERNLIB_XROOTD)
  set(SHIFTSUFFIX "xrootd")
  add_definitions(-DCERNLIB_XROOTD)
endif()
if (CERNLIB_SHIFT)
  set(SHIFTSUFFIX "shift")
  add_definitions(-DCERNLIB_SHIFT)
endif()
if(CERNLIB_ENABLE_TEST)
  ENABLE_TESTING()
  if (CERNLIB_ENABLE_MEMORYTEST)
    find_program(VG NAMES valgrind REQUIRED)
    set(VGOPTIONS "--tool=memcheck --leak-check=full")
  endif()
endif()
###########Respect HFS, e.g. defines lib or lib64 when it is needed.####
include("GNUInstallDirs")
if(CMAKE_SYSTEM_NAME MATCHES Darwin)
  if (NOT CMAKE_MACOSX_RPATH)
    set(CMAKE_MACOSX_RPATH 1)
  endif()
  message(STATUS "CERNLIB: CMAKE_MACOSX_RPATH  ${CMAKE_MACOSX_RPATH}. Set by default to 1. Set to 0 to disable RPATH in the build executables.")
endif()
#### Modules ###########################################################
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules ${CMAKE_MODULE_PATH})
#### RPATH handling ####################################################
set(CMAKE_MACOSX_RPATH 1)
# use, i.e. don't skip the full RPATH for the build tree
SET(CMAKE_SKIP_BUILD_RPATH  FALSE)
# when building, don't use the install RPATH already (but later on when installing)
SET(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)
SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}")
# add the automatically determined parts of the RPATH
# which point to directories outside the build tree to the install RPATH
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
# the RPATH to be used when installing, but only if it's not a system directory
LIST(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}" isSystemDir)
IF("${isSystemDir}" STREQUAL "-1")
   SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}")
ENDIF("${isSystemDir}" STREQUAL "-1")
###################Windows##############################################
set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
###################FLAGS################################################
SET(CMAKE_POSITION_INDEPENDENT_CODE ${OLD_CMAKE_POSITION_INDEPENDENT_CODE})
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
message(STATUS "CERNLIB: Fortran_COMPILER_NAME=${Fortran_COMPILER_NAME}  CMAKE_Fortran_COMPILER_ID=${CMAKE_Fortran_COMPILER_ID}  CMAKE_Fortran_COMPILER_VERSION=${CMAKE_Fortran_COMPILER_VERSION}")
get_filename_component (C_COMPILER_NAME ${CMAKE_C_COMPILER} NAME)
message(STATUS "CERNLIB: C_COMPILER_NAME=${C_COMPILER_NAME}  CMAKE_C_COMPILER_ID=${CMAKE_C_COMPILER_ID}   CMAKE_C_COMPILER_VERSION=${CMAKE_C_COMPILER_VERSION}")
########################################################################
if (APPLE)
  find_program ( SED NAMES gsed REQUIRED)
  find_program ( CPP NAMES gcc REQUIRED)
  find_program ( CPP NAMES gfortran REQUIRED)
endif()
if (LINUX)
  find_program ( SED NAMES sed REQUIRED)
  find_program ( CPP NAMES gcc REQUIRED)
  find_program ( CPP NAMES gfortran REQUIRED)
endif()
if (SunOS)
  find_program ( SED NAMES gsed sed REQUIRED)
  find_program ( CPP NAMES cpp gcpp clang REQUIRED)
endif()
message(STATUS "CERNLIB: SED=${SED}")
########################################################################
function(list_TRANSFORM var bla prefix)
  set(temp "")
  foreach(f ${${var}})
    list(APPEND temp "${prefix}${f}")
  endforeach()
  set(${var} "${temp}" PARENT_SCOPE)
endfunction()
########################################################################
FUNCTION(list_JOIN LISTNAME GLUE OUTP)
  SET(_TMP_STR "")
  FOREACH(VAL ${${LISTNAME}})
    SET(_TMP_STR "${_TMP_STR}${GLUE}${VAL}")
  ENDFOREACH(VAL ${${LISTNAME}})
  STRING(LENGTH "${GLUE}" GLUE_LEN)
  STRING(LENGTH "${_TMP_STR}" OUT_LEN)
  MATH(EXPR OUT_LEN ${OUT_LEN}-${GLUE_LEN})
#  STRING(SUBSTRING "${_TMP_STR}" ${GLUE_LEN} ${OUT_LEN} _TMP_STR)
  SET(${OUTP} "${_TMP_STR}" PARENT_SCOPE)
ENDFUNCTION()
########################################################################
macro(CopyFile a b)
  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${a} ${CMAKE_CURRENT_BINARY_DIR}/${b})
endmacro()
########################################################################
macro(PreprocessFile a b)
string(REPLACE "/" "_" bb "${b}")
add_custom_target(${bb} ALL BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/${b}
                        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${a}
                        COMMAND ${CPP} -x c -E -traditional  -o ${CMAKE_CURRENT_BINARY_DIR}/${b} ${CMAKE_CURRENT_SOURCE_DIR}/${a}
                       WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                       )
endmacro(PreprocessFile a b)
########################################################################
macro(CLATestTarget ast)
  string(REPLACE " " ";" a ${ast})
  list(GET a 0 exec)
  list(REMOVE_AT a 0)
  list(FIND a "<" NEEDSHELL)
  list(JOIN a " " MYCMD)
  if (NEEDSHELL STREQUAL "-1")
    add_test(NAME test${exec} COMMAND  ${CMAKE_CURRENT_BINARY_DIR}/${exec} ${MYCMD}           WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
    if (CERNLIB_ENABLE_MEMORYTEST)
       add_test(NAME memory_test${exec} COMMAND ${VG} ${VGOPTIONS} ${CMAKE_CURRENT_BINARY_DIR}/${exec} ${MYCMD} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
       set_tests_properties(memory_test${exec} PROPERTIES TIMEOUT 500) 
    endif()
  else()
    add_test(NAME test${exec} COMMAND sh -c "${CMAKE_CURRENT_BINARY_DIR}/${exec} ${MYCMD}"  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
    if (CERNLIB_ENABLE_MEMORYTEST)
      add_test(NAME memory_test${exec} COMMAND sh -c "${VG} ${VGOPTIONS} ${CMAKE_CURRENT_BINARY_DIR}/${exec} ${MYCMD}"  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
      set_tests_properties(memory_test${exec} PROPERTIES TIMEOUT 500) 
    endif()
  endif()
  set_tests_properties(test${exec} PROPERTIES TIMEOUT 100) 
endmacro(CLATestTarget)
########################################################################
macro(CLBTestTarget ast)
  string(REPLACE " " ";" a ${ast})
  list(GET a 0 exec)
  list(REMOVE_AT a 0)
  list(FIND a "<" NEEDSHELL)
  list(JOIN a " " MYCMD)
  if (NEEDSHELL STREQUAL "-1")
    if (CERNLIB_ENABLE_MEMORYTEST)
      add_test(NAME memory_test${exec} COMMAND ${VG} ${VGOPTIONS} ${CMAKE_CURRENT_BINARY_DIR}/${exec}/${exec} ${MYCMD}           WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${exec})
      set_tests_properties(memory_test${exec} PROPERTIES TIMEOUT 500) 
    endif()
    add_test(NAME test${exec} COMMAND ${CMAKE_CURRENT_BINARY_DIR}/${exec}/${exec} ${MYCMD}           WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${exec})
  else()
    if (CERNLIB_ENABLE_MEMORYTEST)
      add_test(NAME memory_test${exec} COMMAND sh -c "${VG} ${VGOPTIONS} ${CMAKE_CURRENT_BINARY_DIR}/${exec}/${exec} ${MYCMD}"  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${exec})
      set_tests_properties(memory_test${exec} PROPERTIES TIMEOUT 500) 
    endif()
    add_test(NAME test${exec} COMMAND sh -c "${CMAKE_CURRENT_BINARY_DIR}/${exec}/${exec} ${MYCMD}"  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${exec})
  endif()
  set_tests_properties(test${exec} PROPERTIES TIMEOUT 100) 
endmacro(CLBTestTarget)
########################################################################
macro(CLCernlibFortranProgramTarget)
  set(list_var "${ARGN}")
  list(GET list_var 0 a)
  list(REMOVE_AT list_var 0)
  list(GET list_var 0 bst)
  list(REMOVE_AT list_var 0)
  list(GET list_var 0 d)
  list(REMOVE_AT list_var 0)
  string(REPLACE " " ";" b ${bst})
  list(TRANSFORM b PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/")
  add_executable(${a} ${b})
  target_include_directories(${a} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/)
  target_include_directories(${a} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../)
  target_include_directories(${a} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../../)
  if (APPLE OR (${d} STREQUAL "A") )
    target_link_libraries(${a} PRIVATE ${list_var} ${libxrdposix})
  else()
    target_link_libraries(${a} PRIVATE -Wl,--start-group ${list_var} -Wl,--end-group ${libxrdposix})
  endif()
endmacro(CLCernlibFortranProgramTarget)
########################################################################
macro(CLGeantFortranProgramTarget a b c d e f g)
  set( ASOURCE ${${a}_SRCS_F})
  list(TRANSFORM ASOURCE PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/${a}/")
  add_executable(${a} ${ASOURCE})
  make_directory(${CMAKE_CURRENT_BINARY_DIR}/${a})
  set_target_properties(${a} PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${a})
  target_include_directories(${a} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/)
  target_include_directories(${a} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../)
  target_include_directories(${a} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../../)
if (APPLE)
  target_link_libraries(${a} PRIVATE ${b} ${c} ${d} ${e} ${f} ${g}  mathlib_static ${X11_Xt_LIB} ${X11_LIBRARIES} ${libxrdposix})
else()
  target_link_libraries(${a} PRIVATE -Wl,--start-group ${b} ${c} ${d} ${e} ${f} ${g} mathlib_static -Wl,--end-group ${X11_Xt_LIB} ${X11_LIBRARIES} ${libxrdposix} )
endif()
endmacro(CLGeantFortranProgramTarget)
########################################################################
macro(CLEpioFortranProgramTarget a b c d e)
  add_executable(${a} ${${a}_SRCS_F})
  set_target_properties(${a} PROPERTIES  RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${a})
  target_include_directories(${a} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/)
  target_include_directories(${a} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../)
  target_include_directories(${a} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../../)
if (APPLE)
  target_link_libraries(${a} PRIVATE ${b} ${c} ${d} ${e} ${libxrdposix})
else()
  target_link_libraries(${a} PRIVATE -Wl,--start-group ${b} ${c} ${d} ${e} -Wl,--end-group  ${libxrdposix})
endif()
endmacro(CLEpioFortranProgramTarget)
########################################################################
macro(cdf_compile a b)
     add_custom_command(OUTPUT ${b}
                        DEPENDS ${a}  kuipc
                        COMMAND ${CMAKE_BINARY_DIR}/kuipc -c ${a} ${b}
                       WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                       )
    set_source_files_properties(${b} PROPERTIES GENERATED ON)
endmacro(cdf_compile)
########################################################################
macro(set_package_flags a )
  if ( ${a} STREQUAL "code_motif" )
    include_directories(${FREETYPE_INCLUDE_DIRS})
  endif()
  if ( ${a} STREQUAL "graflib" )
    include_directories(${FREETYPE_INCLUDE_DIRS})
  endif()
  if (${a} STREQUAL "paw_motif" )
    include_directories(${FREETYPE_INCLUDE_DIRS})
  endif()
  if ( ${a} STREQUAL "packlib" )
  else()
    SET(CERNLIB_MMAP 1)
#    SET(CERNLIB_DOUBLE 1)
#    SET(CERNLIB_MMAP 1)
#    add_definitions(-DCERNLIB_DOUBLE -DCERNLIB_QX_SC -DCERNLIB_MMAP -DLINUX)
  endif()
endmacro(set_package_flags)
########################################################################
macro(install_symlink filepath sympath)
     get_filename_component(LINKDIR  ${sympath} DIRECTORY)
     make_directory(${CMAKE_CURRENT_BINARY_DIR}/symlinks/${LINKDIR})
     string(MD5 DUMMY ${filepath}_${sympath})
     add_custom_TARGET( ${DUMMY} ALL BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/symlinks/${sympath}
                         COMMAND ${CMAKE_COMMAND} -E create_symlink ${filepath} ${CMAKE_CURRENT_BINARY_DIR}/symlinks/${sympath})
     INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/symlinks/${sympath} DESTINATION ${LINKDIR})
endmacro(install_symlink)
########################################################################
# set architecture flags
if(${CMAKE_SYSTEM_PROCESSOR} MATCHES "x86_64")
  if (CERNLIB_FORCE_32)
    add_definitions(-D__i386__)
  else()
    add_definitions(-D__X86_64__)
  endif()
else(${CMAKE_SYSTEM_PROCESSOR} MATCHES "aarch64" OR ${CMAKE_SYSTEM_PROCESSOR} MATCHES "arm64")
  add_definitions(-D__aarch64__)
endif()
########################################################################
#Set compiler flags and suffixes
if (EXISTS ${PROJECT_SOURCE_DIR}/cmake/Modules/CERNLIB_${CMAKE_Fortran_COMPILER_ID}_Fortran.cmake)
  include(CERNLIB_${CMAKE_Fortran_COMPILER_ID}_Fortran)
endif()
if (EXISTS ${PROJECT_SOURCE_DIR}/cmake/Modules/CERNLIB_${CMAKE_C_COMPILER_ID}_C.cmake)
  include(CERNLIB_${CMAKE_C_COMPILER_ID}_C)
endif()
if(CMAKE_SYSTEM_NAME MATCHES Darwin)
#  set(CMAKE_SHARED_LIBRARY_CREATE_Fortran_FLAGS "${CMAKE_SHARED_LIBRARY_CREATE_Fortran_FLAGS} -flat_namespace -single_module -undefined dynamic_lookup")
  include(CERNLIB_${CMAKE_Fortran_COMPILER_ID}_Fortran)
  include(CERNLIB_${CMAKE_C_COMPILER_ID}_C)  
endif()
if (CERNLIB_NO_SUFFIX)
  set ( COMPSUFFIX "" )
  set ( COMPSUFFIXBIN "" )
endif()
message(STATUS "CERNLIB: COMPSUFFIX=${COMPSUFFIX} COMPSUFFIXBIN=${COMPSUFFIXBIN}")

#include "paw/quest.inc"
#Linux has here -Wl,-z,relro -Wl,--as-needed  -Wl,-z,now -specs=/usr/lib/rpm/redhat/redhat-hardened-ld -specs=/usr/lib/rpm/redhat/redhat-annobin-cc1
set(CMAKE_EXE_LINKER_FLAGS "")
set(CMAKE_MODULE_LINKER_FLAGS  "")
set(CMAKE_SHARED_LINKER_FLAGS  "")
set(CMAKE_STATIC_LINKER_FLAGS  "")

if (APPLE)
 set (CMAKE_EXE_LINKER_FLAGS  "-v -Wl,-ld_classic,-dead_strip")
 set (CMAKE_EXE_LINKER_FLAGS_DEBUG  "-v -Wl,-ld_classic,-dead_strip")
endif()

set(CMAKE_EXE_LINKER_FLAGS_${CMAKE_BUILD_TYPE}  "")
set(CMAKE_MODULE_LINKER_FLAGS_${CMAKE_BUILD_TYPE}  "")
set(CMAKE_SHARED_LINKER_FLAGS_${CMAKE_BUILD_TYPE}  "")
set(CMAKE_STATIC_LINKER_FLAGS_${CMAKE_BUILD_TYPE}  "")

set(CMAKE_Fortran_FLAGS_${CMAKE_BUILD_TYPE}  "")
set(CMAKE_C_FLAGS_${CMAKE_BUILD_TYPE}  "")

message(STATUS "CERNLIB: CMAKE_EXE_LINKER_FLAGS=${CMAKE_EXE_LINKER_FLAGS}")
message(STATUS "CERNLIB: CMAKE_MODULE_LINKER_FLAGS=${CMAKE_MODULE_LINKER_FLAGS}")
message(STATUS "CERNLIB: CMAKE_SHARED_LINKER_FLAGS=${CMAKE_SHARED_LINKER_FLAGS}")
message(STATUS "CERNLIB: CMAKE_STATIC_LINKER_FLAGS=${CMAKE_STATIC_LINKER_FLAGS}")
message(STATUS "CERNLIB: CMAKE_EXE_LINKER_FLAGS_${CMAKE_BUILD_TYPE}=${CMAKE_EXE_LINKER_FLAGS_${CMAKE_BUILD_TYPE}}")
message(STATUS "CERNLIB: CMAKE_MODULE_LINKER_FLAGS_${CMAKE_BUILD_TYPE}=${CMAKE_MODULE_LINKER_FLAGS_${CMAKE_BUILD_TYPE}}")
message(STATUS "CERNLIB: CMAKE_SHARED_LINKER_FLAGS_${CMAKE_BUILD_TYPE}=${CMAKE_SHARED_LINKER_FLAGS_${CMAKE_BUILD_TYPE}}")
message(STATUS "CERNLIB: CMAKE_STATIC_LINKER_FLAGS_${CMAKE_BUILD_TYPE}=${CMAKE_STATIC_LINKER_FLAGS_${CMAKE_BUILD_TYPE}}")
message(STATUS "CERNLIB: CMAKE_Fortran_FLAGS_${CMAKE_BUILD_TYPE}=${CMAKE_Fortran_FLAGS_${CMAKE_BUILD_TYPE}}")
message(STATUS "CERNLIB: CMAKE_C_FLAGS_${CMAKE_BUILD_TYPE}=${CMAKE_C_FLAGS_${CMAKE_BUILD_TYPE}}")
message(STATUS "CERNLIB: CPP=${CPP}")

include(CheckFortranCompilerFlag)
include(CheckCCompilerFlag)
set ( NEW_CMAKE_C_FLAGS " ")

set(_LIST_CMAKE_C_FLAGS "${C_C_FLAGS_TO_CHECK} ${CMAKE_C_FLAGS}")
string(REPLACE "-isystem " "-I" LIST_CMAKE_C_FLAGS "${_LIST_CMAKE_C_FLAGS}")
set(_LIST_CMAKE_C_FLAGS "${LIST_CMAKE_C_FLAGS}")
string(REPLACE "-I " "-I" LIST_CMAKE_C_FLAGS "${_LIST_CMAKE_C_FLAGS}")
set(_LIST_CMAKE_C_FLAGS "${LIST_CMAKE_C_FLAGS}")
string(REPLACE " " ";" LIST_CMAKE_C_FLAGS "${_LIST_CMAKE_C_FLAGS}")
set(_LIST_CMAKE_C_FLAGS "${LIST_CMAKE_C_FLAGS}")

foreach(fx IN LISTS LIST_CMAKE_C_FLAGS)
  if (fx)
    check_c_compiler_flag(${fx} C_COMPILER_SUPPORTS_${fx})
    if(C_COMPILER_SUPPORTS_${fx})
      list( APPEND NEW_CMAKE_C_FLAGS ${fx})
      set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${fx}" ) #neede for dependent flags
    endif()
  endif()
endforeach()
IF(CMAKE_VERSION VERSION_GREATER 3.5.99)
  list(FILTER NEW_CMAKE_C_FLAGS  EXCLUDE REGEX "flto.*")
  list(FILTER NEW_CMAKE_C_FLAGS  EXCLUDE REGEX "ffat.*")
  list(FILTER NEW_CMAKE_C_FLAGS  EXCLUDE REGEX "-O3")
#It would be nice to implement the filtering as well
endif()
list(JOIN NEW_CMAKE_C_FLAGS " " CMAKE_C_FLAGS_X )
if (NOT CMAKE_FORCE_FLAGS)
  set(CMAKE_C_FLAGS ${CMAKE_C_FLAGS_X})
endif()
if (CERNLIB_FORCE_32)
  add_definitions(-DCERNLIB_32BITCROSS)
  if (CMAKE_C_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_C_FLAGS " -m32 ${CMAKE_C_FLAGS}")
  endif()
endif()

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS}" CACHE STRING "Flags for the C compiler")
message(STATUS "CERNLIB: CMAKE_C_FLAGS=${CMAKE_C_FLAGS}")

########################################################################
set ( NEW_CMAKE_Fortran_FLAGS " ")

set(_LIST_CMAKE_Fortran_FLAGS "${F_Fortran_FLAGS_TO_CHECK} ${CMAKE_Fortran_FLAGS}")
string(REPLACE "-isystem " "-I" LIST_CMAKE_Fortran_FLAGS "${_LIST_CMAKE_Fortran_FLAGS}")
set(_LIST_CMAKE_Fortran_FLAGS "${LIST_CMAKE_Fortran_FLAGS}")
string(REPLACE "-I " "-I" LIST_CMAKE_Fortran_FLAGS "${_LIST_CMAKE_Fortran_FLAGS}")
set(_LIST_CMAKE_Fortran_FLAGS "${LIST_CMAKE_Fortran_FLAGS}")
string(REPLACE " " ";" LIST_CMAKE_Fortran_FLAGS "${_LIST_CMAKE_Fortran_FLAGS}")
set(_LIST_CMAKE_Fortran_FLAGS "${LIST_CMAKE_Fortran_FLAGS}")

foreach(fl IN LISTS LIST_CMAKE_Fortran_FLAGS)
  if (fl)
    check_Fortran_compiler_flag(${fl} Fortran_COMPILER_SUPPORTS_${fl})
    if(Fortran_COMPILER_SUPPORTS_${fl})
      list( APPEND NEW_CMAKE_Fortran_FLAGS ${fl})
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${fl}" ) #needed for dependent flags
    endif()
  endif()
endforeach()
IF(CMAKE_VERSION VERSION_GREATER 3.5.99)
  list(FILTER NEW_CMAKE_Fortran_FLAGS  EXCLUDE REGEX "flto.*")
  list(FILTER NEW_CMAKE_Fortran_FLAGS  EXCLUDE REGEX "ffat.*")
  list(FILTER NEW_CMAKE_Fortran_FLAGS  EXCLUDE REGEX "-O3")
  list(FILTER NEW_CMAKE_Fortran_FLAGS  EXCLUDE REGEX "-Werror=.*")
#It would be nice to implement the filtering as well
endif()
list(JOIN NEW_CMAKE_Fortran_FLAGS " " CMAKE_Fortran_FLAGS_X )
if (NOT CMAKE_FORCE_FLAGS)
  set(CMAKE_Fortran_FLAGS ${CMAKE_Fortran_FLAGS_X})
endif()
if (CERNLIB_FORCE_32)
  add_definitions(-DCERNLIB_32BITCROSS)
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_Fortran_FLAGS " -m32 ${CMAKE_Fortran_FLAGS}")
  endif()
endif()
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}" CACHE STRING "Flags for the Fortran compiler")
message(STATUS "CERNLIB : CMAKE_Fortran_FLAGS=${CMAKE_Fortran_FLAGS}")

########################################################################
set(man1  cernlib.1  cernlib-static.1  dzedit.1  dzeX11.1  gxint.1  kuesvr.1  kuipc.1  kxterm.1  nycheck.1  nydiff.1  nyindex.1  nylist.1
          nymerge.1  nypatchy.1  nyshell.1  nysynopt.1  nytidy.1  paw++.1  paw.1  pawX11.1  yexpand.1  ypatchy.1  zftp.1)
set(man8  pawserv.8  zserv.8)

foreach ( m ${man1} )
IF(CMAKE_VERSION VERSION_GREATER 3.18.0)
  file(ARCHIVE_CREATE OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${m}.gz PATHS ${CMAKE_CURRENT_SOURCE_DIR}/contrib/man/man1/${m} COMPRESSION GZip)
else()
    add_custom_target( ${m}gz ALL
        COMMAND ${CMAKE_COMMAND} -E tar "cfz" ${CMAKE_CURRENT_BINARY_DIR}/${m}.gz --format=gnutar -- ${CMAKE_CURRENT_SOURCE_DIR}/contrib/man/man1/${m}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}"
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/contrib/man/man1/${m}
        COMMENT "GZipping to ${CMAKE_CURRENT_BINARY_DIR}/${m}.gz."
    )
endif()
  install(FILES  ${CMAKE_CURRENT_BINARY_DIR}/${m}.gz DESTINATION ${CMAKE_INSTALL_DATADIR}/man/man1)
endforeach ( m ${man1} )

foreach ( m ${man8} )
IF(CMAKE_VERSION VERSION_GREATER 3.18.0)
  file(ARCHIVE_CREATE OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${m}.gz PATHS ${CMAKE_CURRENT_SOURCE_DIR}/contrib/man/man8/${m} COMPRESSION GZip)
else()
    add_custom_target( ${m}gz ALL
        COMMAND ${CMAKE_COMMAND} -E tar "cfz" ${CMAKE_CURRENT_BINARY_DIR}/${m}.gz --format=gnutar -- ${CMAKE_CURRENT_SOURCE_DIR}/contrib/man/man8/${m}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}"
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/contrib/man/man8/${m}
        COMMENT "GZipping to ${CMAKE_CURRENT_BINARY_DIR}/${m}.gz."
    )
endif()
  install(FILES  ${CMAKE_CURRENT_BINARY_DIR}/${m}.gz DESTINATION ${CMAKE_INSTALL_DATADIR}/man/man8)
endforeach ( m ${man8} )

#install(PROGRAMS ${CMAKE_CURRENT_SOURCE_DIR}/patchy5/yexpand.script  DESTINATION ${CMAKE_INSTALL_BINDIR}/ RENAME yexpand.script${COMPSUFFIXBIN} )
set ( CERN ${CMAKE_INSTALL_PREFIX}/lib${LIB_SUFFIX}/cernlib/)

########################################################################
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/contrib/cernlib.in FILE_CONTENTS)
string(REPLACE "@PREFIX@" "${CMAKE_INSTALL_PREFIX}" FILE_CONTENTS "${FILE_CONTENTS}")
string(REPLACE "@LIBPREFIX@" "" FILE_CONTENTS "${FILE_CONTENTS}")
string(REPLACE "@CERN@" "${CMAKE_INSTALL_PREFIX}/lib${LIB_SUFFIX}/cernlib" FILE_CONTENTS "${FILE_CONTENTS}")
string(REPLACE "@VERSION@" "${CERNLIB_VERSION_MAJOR}" FILE_CONTENTS "${FILE_CONTENTS}")
string(REPLACE "-lgfortran" "" FILE_CONTENTS "${FILE_CONTENTS}")
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/cernlib "${FILE_CONTENTS}")
install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/cernlib DESTINATION ${CMAKE_INSTALL_BINDIR})
########################################################################
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/contrib/cernlib.sh.in FILE_CONTENTS)
string(REPLACE "==CERN==" "${CERN}" FILE_CONTENTS "${FILE_CONTENTS}")
string(REPLACE "==CERN_LEVEL==" "${CERNLIB_VERSION_MAJOR}" FILE_CONTENTS "${FILE_CONTENTS}")
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/cernlib-${CERNLIB_VERSION}.sh "${FILE_CONTENTS}")
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/cernlib-${CERNLIB_VERSION}.sh DESTINATION ${CMAKE_INSTALL_DATADIR}/doc/cernlib-utils)
########################################################################
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/contrib/cernlib.csh.in FILE_CONTENTS)
string(REPLACE "==CERN==" "${CERN}" FILE_CONTENTS "${FILE_CONTENTS}")
string(REPLACE "==CERN_LEVEL==" "${CERNLIB_VERSION_MAJOR}" FILE_CONTENTS "${FILE_CONTENTS}")
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/cernlib-${CERNLIB_VERSION}.csh "${FILE_CONTENTS}")
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/cernlib-${CERNLIB_VERSION}.csh DESTINATION ${CMAKE_INSTALL_DATADIR}/doc/cernlib-utils)
########################################################################
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/scripts/cernlib FILE_CONTENTS)
string(REPLACE "@PREFIX@" "${CMAKE_INSTALL_PREFIX}/" FILE_CONTENTS "${FILE_CONTENTS}")
string(REPLACE "\"/cern\"" "\"${CMAKE_INSTALL_PREFIX}/lib${LIB_SUFFIX}/cernlib\"" FILE_CONTENTS "${FILE_CONTENTS}")
string(REPLACE "\"pro\"" "\"${CERNLIB_VERSION_MAJOR}\"" FILE_CONTENTS "${FILE_CONTENTS}")
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/scripts/cernlib "${FILE_CONTENTS}")
install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/scripts/cernlib DESTINATION ${CMAKE_INSTALL_BINDIR}/ RENAME cernlib-static)
########################################################################
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/scripts/gxint FILE_CONTENTS)
string(REPLACE "@PREFIX@" "${CMAKE_INSTALL_PREFIX}" FILE_CONTENTS "${FILE_CONTENTS}")
string(REPLACE "\"/cern\"" "\"${CMAKE_INSTALL_PREFIX}/lib${LIB_SUFFIX}/cernlib\"" FILE_CONTENTS "${FILE_CONTENTS}")
string(REPLACE "\"pro\"" "${CERNLIB_VERSION_MAJOR}" FILE_CONTENTS "${FILE_CONTENTS}")
string(REPLACE "LDN=\"f77\"" "LDN=\"gfortran\"" FILE_CONTENTS "${FILE_CONTENTS}")
string(REPLACE "\$\{CERN\}/\$\{ver\}/lib/gxint\$\{gvs\}.$_o" "${CERN}/${CERNLIB_VERSION_MAJOR}/gxint.f" FILE_CONTENTS "${FILE_CONTENTS}")
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/scripts/gxint "${FILE_CONTENTS}")
install(FILES  ${CMAKE_CURRENT_BINARY_DIR}/scripts/gxint DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs  PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
########################################################################
install(PROGRAMS ${CMAKE_CURRENT_SOURCE_DIR}/patchy5/yexpand.script  DESTINATION ${CMAKE_INSTALL_BINDIR} RENAME yexpand${COMPSUFFIXBIN} COMPONENT libs)
########################################################################
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/graflib/dzdoc/dzedit/dzedit.script FILE_CONTENTS)
string(REPLACE "\$drv " "\$drv${COMPSUFFIX} " FILE_CONTENTS "${FILE_CONTENTS}")
string(REPLACE "\"/cern\"" "\"${CMAKE_INSTALL_PREFIX}/lib${LIB_SUFFIX}/cernlib\"" FILE_CONTENTS "${FILE_CONTENTS}")
string(REPLACE "\"pro\"" "\"${CERNLIB_VERSION_MAJOR}\"" FILE_CONTENTS "${FILE_CONTENTS}")
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/graflib/dzdoc/dzedit/dzedit.script "${FILE_CONTENTS}")
install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/graflib/dzdoc/dzedit/dzedit.script  DESTINATION ${CMAKE_INSTALL_BINDIR} RENAME dzedit${COMPSUFFIXBIN} COMPONENT libs)
########################################################################
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/pawlib/paw/programs/paw.script FILE_CONTENTS)
string(REPLACE "\$drv " "\$drv${COMPSUFFIX} " FILE_CONTENTS "${FILE_CONTENTS}")
string(REPLACE "\"/cern\"" "\"${CMAKE_INSTALL_PREFIX}/lib${LIB_SUFFIX}/cernlib\"" FILE_CONTENTS "${FILE_CONTENTS}")
# string(REPLACE "\"pro\"" "\"${CERNLIB_VERSION_MAJOR}\"" FILE_CONTENTS "${FILE_CONTENTS}")
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/pawlib/paw/programs/paw.script "${FILE_CONTENTS}")
install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/pawlib/paw/programs/paw.script  DESTINATION ${CMAKE_INSTALL_BINDIR} RENAME paw${COMPSUFFIXBIN} COMPONENT libs)
########################################################################
add_custom_target( gxint321f ALL BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/gxint321.f
                        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/geant321/gxint/gxint.F
                        COMMAND ${CPP} -x c -E -traditional  -o ${CMAKE_CURRENT_BINARY_DIR}/gxint321.f -I ${CMAKE_CURRENT_SOURCE_DIR}/geant321 ${CMAKE_CURRENT_SOURCE_DIR}/geant321/gxint/gxint.F
                        COMMAND  ${SED} -i -e "/^$$/d" -e "/^#  *[0-9][0-9]*  *.*$$/d" -e "/^XCOMM$$/s//#/" -e "/^XCOMM[^a-zA-Z0-9_]/s/^XCOMM/#/" -e "/^# [0-9][0-9]/d" -e "/^#line /d"   ${CMAKE_CURRENT_BINARY_DIR}/gxint321.f
                      # WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                       )
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/gxint321.f DESTINATION ${CMAKE_INSTALL_INCLUDEDIR} COMPONENT libs  )
install_symlink(gxint321.f ${CMAKE_INSTALL_INCLUDEDIR}/gxint.f )
########################################################################
install(FILES
${CMAKE_CURRENT_SOURCE_DIR}/contrib/pixmaps/kxterm48x48.xpm
${CMAKE_CURRENT_SOURCE_DIR}/contrib/pixmaps/paw48x48.xpm
${CMAKE_CURRENT_SOURCE_DIR}/contrib/pixmaps/paw32x32.xpm
${CMAKE_CURRENT_SOURCE_DIR}/contrib/pixmaps/kxterm32x32.xpm
DESTINATION ${CMAKE_INSTALL_DATADIR}/pixmaps)
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/contrib/icons/hicolor DESTINATION ${CMAKE_INSTALL_DATADIR}/icons)

install(FILES
${CMAKE_CURRENT_SOURCE_DIR}/contrib/X11/app-defaults/Geant++
${CMAKE_CURRENT_SOURCE_DIR}/contrib/X11/app-defaults/KxGeant++
${CMAKE_CURRENT_SOURCE_DIR}/contrib/X11/app-defaults/KxPaw++
${CMAKE_CURRENT_SOURCE_DIR}/contrib/X11/app-defaults/KXterm
${CMAKE_CURRENT_SOURCE_DIR}/contrib/X11/app-defaults/Paw++
DESTINATION
${CMAKE_INSTALL_DATADIR}/X11/app-defaults)

install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/contrib/vim DESTINATION ${CMAKE_INSTALL_DATADIR}/doc/cernlib)
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/cfortran/Examples DESTINATION ${CMAKE_INSTALL_DATADIR}/doc/cernlib-devel/cfortran PATTERN "*makefile*" EXCLUDE PATTERN "*fcn\.f*" EXCLUDE)
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/contrib/zftp.README DESTINATION ${CMAKE_INSTALL_DATADIR}/doc/cernlib-packlib${COMPSUFFIXBIN} )
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/contrib/comis-64bit-example.F DESTINATION ${CMAKE_INSTALL_DATADIR}/doc/cernlib-devel )
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/contrib/cernlib.m4 DESTINATION ${CMAKE_INSTALL_DATADIR}/aclocal)
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/geant321/data/xsneut95.dat ${CMAKE_CURRENT_SOURCE_DIR}/mclibs/eurodec/data/eurodec.dat DESTINATION ${CMAKE_INSTALL_DATADIR}/cernlib/${CERNLIB_VERSION})

make_directory(${CMAKE_CURRENT_BINARY_DIR}/contrib)
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/contrib/cernlib.copyright FILE_CONTENTS)
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/contrib/cernlib.copyright "${FILE_CONTENTS}")

install(FILES
${CMAKE_CURRENT_BINARY_DIR}/contrib/cernlib.copyright
${CMAKE_CURRENT_SOURCE_DIR}/contrib/geant321.README
DESTINATION
${CMAKE_INSTALL_DATADIR}/doc/cernlib)
########################################################################
set(kuipc_SRCS_C kuipcc.c kkern.c )
list(TRANSFORM kuipc_SRCS_C PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/packlib/kuip/programs/kuipc/")
add_executable(kuipc ${kuipc_SRCS_C} )
target_include_directories(kuipc PRIVATE ${PROJECT_SOURCE_DIR}/include)
if (APPLE)
  target_link_options(kuipc PRIVATE -mmacosx-version-min=12.0)
   target_compile_options(kuipc PRIVATE  -g -Wall -O0 -mmacosx-version-min=12.0)
endif()
install(TARGETS kuipc DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
target_include_directories(kuipc PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/kuipc)
target_include_directories(kuipc PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/kuip)
set_target_properties(kuipc PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE}  OUTPUT_NAME kuipc)
########################################################################
set(NullParameter )

if (APPLE)
  set(crypto )
  set(crypt )
  set(nsl )
  set(Xm )
  SET(CERNLIB_LINUX 1)
  SET(CERNLIB_LNX 1)
  if (CMAKE_SIZEOF_VOID_P EQUAL 8)
     message (STATUS "Compiling for 64-bit")
     SET(CERNLIB_QMLXIA64 1)
     #What should be here for Darwin?
     add_definitions(-DCERNLIB_QMLXIA64)
  endif()
  SET(CERNLIB_MACOSX 1)
  add_definitions(-Dunix)
#  add_definitions(-DCERNLIB_QX_SC)
  add_definitions(-DCERNLIB_MACOSX)
  #The line below should be replaced at some point
  add_definitions(-DCERNLIB_LINUX)
  add_definitions(-D__DARWIN__)
  add_definitions(-D__APPLE__)
  add_definitions(-DHAVE_MEMMOVE)
endif()
if(LINUX)
  SET(CERNLIB_LINUX 1)
  SET(CERNLIB_LNX 1)
endif()
if(SunOS)
  set(CERNLIB_SOLARIS 1)
endif()
if(LINUX OR SunOS)
  set(crypt crypt)
  if (NOT OPENSSL_CRYPTO_LIBRARY)
  find_package(OpenSSL REQUIRED)
  message(STATUS "CERNLIB: OPENSSL_CRYPTO_LIBRARY=${OPENSSL_CRYPTO_LIBRARY}")
  if (CERNLIB_FORCE_32)
    STRING(REPLACE "lib64" "lib" OPENSSL_CRYPTO_LIBRARY "${OPENSSL_CRYPTO_LIBRARY}")
    message(STATUS "CERNLIB: Guess for 32bit: OPENSSL_CRYPTO_LIBRARY=${OPENSSL_CRYPTO_LIBRARY}")
  endif()
  endif()
  set(crypto ${OPENSSL_CRYPTO_LIBRARY})
  find_library(nsl NAMES nsl nsl2 REQUIRED)
  message(STATUS "CERNLIB: nsl=${nsl}")
  set(Xm Xm)
  set(Xbae Xbae)
  if (CMAKE_SIZEOF_VOID_P EQUAL 8)
     message (STATUS "Compiling for 64-bit")
     SET(CERNLIB_QMLXIA64 1)
     add_definitions(-DCERNLIB_QMLXIA64)
   endif()
  if (NOT CERNLIB_USE_INTERNAL_XBAE)
    SET(CERNLIB_NOLIBXBAE 1)
  endif()
endif()

SET(CERNLIB_UNIX 1)
SET(CERNLIB_X11 1)
SET(CERNLIB_ZEBRA 1)
SET(CERNLIB_FALCO 1)
SET(CERNLIB_GIF 1)
SET(CERNLIB_KERNEL 1)
if (Fortran_COMPILER_NAME STREQUAL g77 OR CMAKE_Fortran_COMPILER_ID STREQUAL "Flang")
if  (CMAKE_Fortran_COMPILER_ID STREQUAL "Flang")
  add_definitions(-DCERNLIB_FLANG)
endif()
#Should something be defined for g77?
#  SET(CERNLIB_F2C 1)#?
else()
  SET(CERNLIB_GFORTRAN 1)
  add_definitions(-DCERNLIB_GFORTRAN)
endif()
SET(CERNLIB_QMGLIBC 1)
SET(CERNLIB_ZBUF 1)
SET(CERNLIB_NTC 1)
SET(CERNLIB_SHL 1)
SET(CERNLIB_BUGREPORT 1)
SET(CERNLIB_PAW 1)
SET(CERNLIB_COMIS 1)
SET(CERNLIB_CG 1)
SET(CERNLIB_MONITOR 1)
SET(CERNLIB_INTERACT 1)
SET(CERNLIB_QF2C 1)
set(CERNLIB_CZ 1)
SET(CERNLIB_MOTIF 1)
set(CERNLIB_DOUBLE 1)
########################################################################
if (LINUX)
  add_definitions(
                  -DCERNLIB_LINUX
                  -DCERNLIB_LNX
                 )
endif()
if (SunOS)
  add_definitions(
                  -DCERNLIB_SOLARIS -DCERNLIB_QMLNX -DSOLARIS -DCERNLIB_QX_SC -DCERNLIB_SYS5 -DCERNLIB_DOUBLE
                 )
endif()
add_definitions(
-DCERNLIB_UNIX
-DCERNLIB_QMGLIBC
-DCERNLIB_QGETCWD
-D_FORTIFY_SOURCE=2
-DCERNLIB_VERBOSE
#-DCERNLIB_X11
#-DCERNLIB_ZEBRA
#-DCERNLIB_FALCO
#-DCERNLIB_GIF
#-DCERNLIB_KERNEL
#-DCERNLIB_ZBUF
#-DCERNLIB_NTC
#-DCERNLIB_SHL
#-DCERNLIB_BUGREPORT
#-DCERNLIB_PAW
#-DCERNLIB_COMIS
#-DCERNLIB_MOTIF
#-DCERNLIB_CG
#-DCERNLIB_MONITOR
#-DCERNLIB_INTERACT
#-DCERNLIB_QF2C
#-DCERNLIB_TESTALL
)
if (Fortran_COMPILER_NAME STREQUAL g77)
#The flags should be checked for validity for g77
  SET (PLINAME "LINUX,UNIX,LNX,QMGLIBC,QMLXIA64,VERBOSE,QGETCWD")
else()
#The flags should be checked for validity for 32 bit
  SET (PLINAME "LINUX,UNIX,LNX,QMGLIBC,QMLXIA64,VERBOSE,QGETCWD,GFORTRAN")
endif()
########################################################################
find_package(X11  REQUIRED)
message(STATUS "CERNLIB: X11_Xt_LIB=${X11_Xt_LIB} X11_Xaw_LIB=${X11_Xaw_LIB} X11_LIBRARIES=${X11_LIBRARIES}")
if (NOT X11_Xaw_LIB)
# 1) The X11R6.6 does not have Xaw, I think.
# 2) cmake<3.19 does not detect Xaw
  find_library(rawxaw NAMES Xaw)
  if (rawxaw)
    set(X11_Xaw_LIB ${rawxaw})
  else()
    set(X11_Xaw_LIB)
  endif()
endif()
if (APPLE)
  set(X11_LIBRARIES -L/opt/X11/lib -L/opt/homebrew/lib ${X11_LIBRARIES})
endif()
message(STATUS "CERNLIB: X11_Xt_LIB=${X11_Xt_LIB} X11_Xaw_LIB=${X11_Xaw_LIB} X11_LIBRARIES=${X11_LIBRARIES}")

if (CERNLIB_USE_INTERNAL_LAPACK)
  add_subdirectory(lapack)
  set (BLAS_LIBRARIES blas_static)
  set (LAPACK_lapack_LIBRARY lapack_static)
else()
  find_package(BLAS REQUIRED)
  find_package(LAPACK REQUIRED)
#One should hanlde the deprecated codes here
#Lapack >= 3.7.0 needs the flag below
  set(LAPACK_COMPILE_DEFINITIONS -DDGEQPF=DGEQP3 -Ddgeqpf=dgeqp3)
endif()

find_package(Freetype REQUIRED)
message(STATUS "CERNLIB: FREETYPE_INCLUDE_DIRS=${FREETYPE_INCLUDE_DIRS} FREETYPE_LIBRARIES=${FREETYPE_LIBRARIES}")
find_package(Motif REQUIRED)
if (APPLE)
  set(X11_INCLUDE_DIR /opt/X11/include /opt/homebrew/include)
else()
  set(Xm ${MOTIF_LIBRARIES})
endif()
include_directories(${X11_INCLUDE_DIR} ${MOTIF_INCLUDE_DIR} ${XROOTD_INCLUDE_DIR})
Message(STATUS "CERNLIB: MOTIF_INCLUDE_DIR=${MOTIF_INCLUDE_DIR} MOTIF_LIBRARIES=${MOTIF_LIBRARIES}")
########################################################################
if (CERNLIB_USE_INTERNAL_XBAE)
  add_subdirectory(paw_motif/xbae)
  set(Xbae xbae_static)
endif()
add_subdirectory(packlib)
add_subdirectory(kernlib)
add_subdirectory(mathlib)
add_subdirectory(graflib/higz)
add_subdirectory(graflib)
add_subdirectory(phtools)
add_subdirectory(pawlib)
add_subdirectory(geant321)
add_subdirectory(paw_motif)
add_subdirectory(mclibs/ariadne)
#add_subdirectory(mclibs/ariadne_407)
add_subdirectory(mclibs/cojets)
add_subdirectory(mclibs/eurodec)
add_subdirectory(mclibs/fritiof)
add_subdirectory(mclibs/herwig)
#add_subdirectory(mclibs/herwig58)
add_subdirectory(mclibs/isajet)
add_subdirectory(mclibs/jetset)
add_subdirectory(mclibs/lepto63)
add_subdirectory(mclibs/pdf)
add_subdirectory(mclibs/photos)
add_subdirectory(mclibs/pythia)
add_subdirectory(code_motif)
########################################################################
set(CERNLIB_MMAP 1)
install(FILES
${CMAKE_CURRENT_SOURCE_DIR}/cfortran/cfortran.doc
${CMAKE_CURRENT_SOURCE_DIR}/cfortran/cfortran.h
${CMAKE_CURRENT_SOURCE_DIR}/cfortran/comis.h
${CMAKE_CURRENT_SOURCE_DIR}/cfortran/cspack.h
${CMAKE_CURRENT_SOURCE_DIR}/cfortran/geant315.h
${CMAKE_CURRENT_SOURCE_DIR}/cfortran/geant321.h
${CMAKE_CURRENT_SOURCE_DIR}/cfortran/gen.h
${CMAKE_CURRENT_SOURCE_DIR}/cfortran/graflib.h
${CMAKE_CURRENT_SOURCE_DIR}/cfortran/hbook_94b.h
${CMAKE_CURRENT_SOURCE_DIR}/cfortran/hbook.h
${CMAKE_CURRENT_SOURCE_DIR}/cfortran/higz.h
${CMAKE_CURRENT_SOURCE_DIR}/cfortran/hplot.h
${CMAKE_CURRENT_SOURCE_DIR}/cfortran/jetset74.h
${CMAKE_CURRENT_SOURCE_DIR}/cfortran/kernlib.h
${CMAKE_CURRENT_SOURCE_DIR}/cfortran/kuip.h
${CMAKE_CURRENT_SOURCE_DIR}/cfortran/lapack.h
${CMAKE_CURRENT_SOURCE_DIR}/cfortran/lepto62.h
${CMAKE_CURRENT_SOURCE_DIR}/cfortran/minuit.h
${CMAKE_CURRENT_SOURCE_DIR}/cfortran/packlib.h
${CMAKE_CURRENT_SOURCE_DIR}/cfortran/paw.h
${CMAKE_CURRENT_SOURCE_DIR}/cfortran/zebra.h
DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/  COMPONENT libs  )

set(head cfortran.doc cfortran.h comis.h cspack.h geant315.h geant321.h gen.h graflib.h hbook_94b.h hbook.h higz.h hplot.h jetset74.h kernlib.h kuip.h lapack.h lepto62.h minuit.h packlib.h paw.h zebra.h )

install(CODE "execute_process(COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_INSTALL_INCLUDEDIR}/cfortran/)")
foreach ( m ${head} )
  install_symlink(../${m} ${CMAKE_INSTALL_INCLUDEDIR}/cfortran/${m} )
endforeach ( m ${head} )

install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/contrib/converter.h  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/paw/  )

set(grafX11 higz)
set(hepdb_SRCS_F cdcdrc.F cdclsh.F cdclsr.F cdexit.F cdextq.F cdfilc.F cdkali.F cdkhlp.F cdkpak.F cdkpwd.F cdktxt.F cdkupk.F cdldc.F
       cdlogc.F cdlsc.F cdmerg.F cdmkdr.F cdobjc.F cdopnc.F cdoutp.F  cdpwd.F cdquit.F cdrmc.F cdrmdc.F cdrmtc.F cdrzld.F cdrzop.F
       cdrzst.F cdseac.F cdsele.F cdtree.F cdverc.F cdwind.F cdzoom.F zabend.F hepdb.F)
cdf_compile(${CMAKE_CURRENT_SOURCE_DIR}/packlib/hepdb/programs/hepdb/cdcdf.cdf ${CMAKE_CURRENT_BINARY_DIR}/cdcdf.cdf.c)
list(TRANSFORM hepdb_SRCS_F PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/packlib/hepdb/programs/hepdb/")
add_executable(hepdb ${hepdb_SRCS_F} ${CMAKE_CURRENT_BINARY_DIR}/cdcdf.cdf.c)
if (APPLE)
target_link_libraries(hepdb  PRIVATE packlib_static kernlib_static graflib_static  ${grafX11}_static
        pawlib_static mathlib_static paw_motif_static code_motif_static
   ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} ${crypt} ${MOTIF_LIBRARIES} ${Xbae} ${X11_Xaw_LIB} ${X11_Xt_LIB} ${X11_LIBRARIES} ${libxrdposix})
else()
target_link_libraries(hepdb PRIVATE  -Wl,--start-group  packlib_static kernlib_static graflib_static  ${grafX11}_static
 pawlib_static mathlib_static paw_motif_static code_motif_static
   ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} ${crypt} ${MOTIF_LIBRARIES} ${Xbae} ${X11_Xaw_LIB} ${X11_Xt_LIB} ${X11_LIBRARIES} ${libxrdposix} -Wl,--end-group )
endif()
target_include_directories(hepdb PRIVATE ${PROJECT_SOURCE_DIR}/include)
install(TARGETS hepdb DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
target_include_directories(hepdb PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/hepdb)
set_target_properties(hepdb PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME "hepdb${COMPSUFFIXBIN}" )
########################################################################
set(cdmake_SRCS_F cdmake.F)
list(TRANSFORM cdmake_SRCS_F PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/packlib/hepdb/programs/cdmake/")
add_executable(cdmake ${cdmake_SRCS_F} )
if (APPLE)
target_link_libraries(cdmake PRIVATE  packlib_static kernlib_static graflib_static  ${grafX11}_static
 mathlib_static ${X11_LIBRARIES} ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
else()
target_link_libraries(cdmake PRIVATE -Wl,--start-group   packlib_static kernlib_static graflib_static  ${grafX11}_static
 mathlib_static -Wl,--end-group ${X11_LIBRARIES} ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
endif()
target_include_directories(cdmake PRIVATE ${PROJECT_SOURCE_DIR}/include)
install(TARGETS cdmake DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
target_include_directories(cdmake PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/hepdb)
set_target_properties(cdmake PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME "cdmake${COMPSUFFIXBIN}" )
########################################################################
set(cdmove_SRCS_F cdmove.F)
list(TRANSFORM cdmove_SRCS_F PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/packlib/hepdb/programs/cdmove/")
add_executable(cdmove  ${cdmove_SRCS_F} )
if (APPLE)
  target_link_libraries(cdmove PRIVATE  packlib_static kernlib_static graflib_static  ${grafX11}_static mathlib_static ${X11_LIBRARIES} ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
else()
  target_link_libraries(cdmove PRIVATE packlib_static kernlib_static graflib_static  ${grafX11}_static  mathlib_static ${X11_LIBRARIES} ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
endif()
target_include_directories(cdmove PRIVATE ${PROJECT_SOURCE_DIR}/include)
install(TARGETS cdmove DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
target_include_directories(cdmove PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/hepdb)
set_target_properties(cdmove PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME cdmove${COMPSUFFIXBIN})
########################################################################
set(kuesvr_SRCS_C kuesvr.c)
list(TRANSFORM kuesvr_SRCS_C PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/packlib/kuip/programs/kuesvr/")
add_executable(kuesvr  ${kuesvr_SRCS_C} )
if (APPLE)
  target_link_libraries(kuesvr PRIVATE packlib_static kernlib_static graflib_static ${grafX11}_static mathlib_static ${X11_LIBRARIES} ${BLAS_LIBRARIES} ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
else()
  target_link_libraries(kuesvr PRIVATE packlib_static kernlib_static graflib_static ${grafX11}_static mathlib_static ${X11_LIBRARIES} ${BLAS_LIBRARIES} ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
endif()
target_include_directories(kuesvr PRIVATE ${PROJECT_SOURCE_DIR}/include)
install(TARGETS kuesvr DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
target_include_directories(kuesvr PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/kuip)
set_target_properties(kuesvr PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME kuesvr${COMPSUFFIXBIN})
########################################################################
set(kxterm_SRCS_C
       actions.c
       dialogs.c help.c inputtrdial.c kuipio.c
       kxpan.c kxterm.c menu.c saveastrdial.c searchdialog.c utils.c
       uxpanel.c uxxt.c kkern.c)
list(TRANSFORM kxterm_SRCS_C PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/packlib/kuip/programs/kxterm/")
add_executable(kxterm ${kxterm_SRCS_C} )
if (APPLE)
  target_link_libraries(kxterm PRIVATE packlib_static kernlib_static graflib_static  ${grafX11}_static mathlib_static
   pawlib_static paw_motif_static code_motif_static
   ${BLAS_LIBRARIES} ${LAPACK_lapack_LIBRARY} ${crypt} ${MOTIF_LIBRARIES} ${Xbae} ${X11_Xaw_LIB} ${X11_Xt_LIB} ${X11_LIBRARIES} ${libxrdposix})
else()
  target_link_libraries(kxterm PRIVATE -Wl,--start-group packlib_static kernlib_static graflib_static  ${grafX11}_static
   mathlib_static pawlib_static paw_motif_static code_motif_static -Wl,--end-group
   ${BLAS_LIBRARIES} ${LAPACK_lapack_LIBRARY} ${crypt} ${MOTIF_LIBRARIES} ${Xbae} ${X11_Xaw_LIB} ${X11_Xt_LIB} ${X11_LIBRARIES} ${libxrdposix})
endif()
target_include_directories(kxterm PRIVATE ${PROJECT_SOURCE_DIR}/include)
install(TARGETS kxterm DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
target_include_directories(kxterm PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/kxterm)
target_include_directories(kxterm PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/kuip)
set_target_properties(kxterm PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME kxterm${COMPSUFFIXBIN})
########################################################################
set(cdbackup_SRCS_F cdbackup.F zabend.F)
list(TRANSFORM cdbackup_SRCS_F PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/packlib/hepdb/programs/cdbackup/")
add_executable(cdbackup  ${cdbackup_SRCS_F} )
if (APPLE)
  target_link_libraries(cdbackup PRIVATE packlib_static kernlib_static graflib_static  ${grafX11}_static mathlib_static ${X11_LIBRARIES} ${BLAS_LIBRARIES} ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
else()
  target_link_libraries(cdbackup PRIVATE packlib_static kernlib_static graflib_static  ${grafX11}_static mathlib_static ${X11_LIBRARIES} ${BLAS_LIBRARIES} ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
endif()
target_include_directories(cdbackup PRIVATE ${PROJECT_SOURCE_DIR}/include)
install(TARGETS cdbackup DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
target_include_directories(cdbackup PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/hepdb)
set_target_properties(cdbackup PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME cdbackup${COMPSUFFIXBIN})
########################################################################
set(cdserv_SRCS_F abend.F cdback.F cdbad.F cdconf.F cddelf.F cdload.F
       cdlogf.F cdsend.F cdserv.F cdstop.F cdtrap.F cdwake.F)
list(TRANSFORM cdserv_SRCS_F PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/packlib/hepdb/programs/cdserv/")
add_executable(cdserv ${cdserv_SRCS_F} )
if (APPLE)
  target_link_libraries(cdserv PRIVATE packlib_static kernlib_static graflib_static  ${grafX11}_static mathlib_static ${X11_LIBRARIES} ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
else()
  target_link_libraries(cdserv PRIVATE packlib_static kernlib_static graflib_static  ${grafX11}_static mathlib_static ${X11_LIBRARIES} ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
endif()
target_include_directories(cdserv PRIVATE ${PROJECT_SOURCE_DIR}/include)
install(TARGETS cdserv DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
target_include_directories(cdserv PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/hepdb)
set_target_properties(cdserv PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME cdserv${COMPSUFFIXBIN})
########################################################################
set(dzeX11_SRCS_F dzebkk.F dzechf.F dzechk.F dzecpp.F dzedel.F dzedff.F
       dzedof.F dzedrw.F dzedrz.F dzedsp.F dzehpe.F dzelal.F
       dzelas.F dzelin.F dzelof.F dzemrz.F dzenpg.F dzeop1.F dzeopl.F
       dzeppa.F dzepsb.F dzepur.F dzeqit.F dzerds.F dzesgp.F dzeurz.F
       dzewem.F dzezer.F dzedit.F)
cdf_compile(${CMAKE_CURRENT_SOURCE_DIR}/graflib/dzdoc/dzedit/dzeditcdf.cdf ${CMAKE_CURRENT_BINARY_DIR}/dzeditcdf.cdf.c)
list(TRANSFORM dzeX11_SRCS_F PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/graflib/dzdoc/dzedit/")
add_executable(dzeX11 ${dzeX11_SRCS_F} ${CMAKE_CURRENT_BINARY_DIR}/dzeditcdf.cdf.c)
if (APPLE)
  target_link_libraries(dzeX11 PRIVATE  graflib_static kernlib_static graflib_static packlib_static ${grafX11}_static mathlib_static ${X11_LIBRARIES} ${BLAS_LIBRARIES} ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
else()
  target_link_libraries(dzeX11 PRIVATE -Wl,--start-group graflib_static kernlib_static graflib_static packlib_static ${grafX11}_static mathlib_static  -Wl,--end-group ${X11_LIBRARIES} ${BLAS_LIBRARIES} ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
endif()

target_include_directories(dzeX11 PRIVATE ${PROJECT_SOURCE_DIR}/include)
install(TARGETS dzeX11 DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
target_include_directories(dzeX11 PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/graflib/dzdoc)
set_target_properties(dzeX11 PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME dzeX11${COMPSUFFIXBIN})
make_directory(${CMAKE_CURRENT_BINARY_DIR}/FOO)
make_directory(${CMAKE_CURRENT_BINARY_DIR}/FOO/bin)
install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/FOO/bin DESTINATION ${CMAKE_INSTALL_LIBDIR}/../)
if (NOT CERNLIB_NO_SUFFIX)
  install_symlink( ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_BINDIR}/dzeX11${COMPSUFFIXBIN} ${CMAKE_INSTALL_LIBDIR}/../bin/dzeX11${COMPSUFFIXBIN} )
endif()
########################################################################
set(pawX11_SRCS_F 0pamain.F)
list(TRANSFORM pawX11_SRCS_F PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/pawlib/paw/programs/")
if (CERNLIB_BUILD_SHARED)
add_executable(pawX11.dynamic ${pawX11_SRCS_F} )
if (APPLE)
  target_link_libraries(pawX11.dynamic PRIVATE pawlib kernlib graflib ${grafX11}
  packlib paw_motif code_motif mathlib ${libxrdposix}
  ${BLAS_LIBRARIES}
  ${nsl} ${LAPACK_lapack_LIBRARY} ${crypt} ${MOTIF_LIBRARIES} ${Xbae} ${X11_Xaw_LIB} ${X11_Xt_LIB} ${X11_LIBRARIES} ${CMAKE_DL_LIBS})
else()
  target_link_libraries(pawX11.dynamic PRIVATE -Wl,--start-group pawlib kernlib graflib ${grafX11}
  packlib paw_motif code_motif mathlib -Wl,--end-group ${libxrdposix}
  ${BLAS_LIBRARIES}
  ${nsl} ${LAPACK_lapack_LIBRARY} ${crypt} ${MOTIF_LIBRARIES} ${Xbae} ${X11_Xaw_LIB} ${X11_Xt_LIB} ${X11_LIBRARIES} ${CMAKE_DL_LIBS})
endif()
target_include_directories(pawX11.dynamic PRIVATE ${PROJECT_SOURCE_DIR}/include )
install(TARGETS pawX11.dynamic DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
set_target_properties(pawX11.dynamic PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME pawX11.dynamic${COMPSUFFIXBIN})
endif()
########################################################################
add_executable(pawX11 ${pawX11_SRCS_F} )
if (APPLE)
 target_link_libraries(pawX11 PRIVATE ${BLAS_LIBRARIES} ${nsl} ${LAPACK_lapack_LIBRARY} ${crypt} ${X11_LIBRARIES} pawlib_static kernlib_static graflib_static ${grafX11}_static ${libxrdposix}
     packlib_static paw_motif_static code_motif_static mathlib_static)
else()
  target_link_libraries(pawX11 PRIVATE -Wl,--start-group pawlib_static kernlib_static graflib_static ${grafX11}_static packlib_static paw_motif_static code_motif_static mathlib_static -Wl,--end-group ${BLAS_LIBRARIES} ${libxrdposix}
     ${nsl} ${LAPACK_lapack_LIBRARY} ${crypt} ${X11_LIBRARIES} ${CMAKE_DL_LIBS})
endif()
target_include_directories(pawX11 PRIVATE ${PROJECT_SOURCE_DIR}/include)
install(TARGETS pawX11 DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
set_target_properties(pawX11 PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME pawX11${COMPSUFFIXBIN})
if (NOT CERNLIB_NO_SUFFIX)
  install_symlink(${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_BINDIR}/pawX11${COMPSUFFIXBIN} ${CMAKE_INSTALL_LIBDIR}/../bin/pawX11${COMPSUFFIXBIN})
endif()
########################################################################
set(paw++_SRCS_F 0pamainm.F)
list(TRANSFORM paw++_SRCS_F PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/pawlib/paw/programs/")
if (CERNLIB_BUILD_SHARED)
add_executable(paw++.dynamic ${paw++_SRCS_F} )
if (APPLE)
target_link_libraries(paw++.dynamic PRIVATE  pawlib kernlib graflib ${grafX11}
 packlib paw_motif code_motif mathlib ${libxrdposix}
   ${BLAS_LIBRARIES} ${nsl} ${LAPACK_lapack_LIBRARY} ${crypt} ${MOTIF_LIBRARIES} ${Xbae} ${X11_Xaw_LIB} ${X11_Xt_LIB} ${X11_LIBRARIES} ${CMAKE_DL_LIBS})
else()
target_link_libraries(paw++.dynamic PRIVATE  -Wl,--start-group pawlib kernlib graflib ${grafX11}
 packlib paw_motif code_motif mathlib  -Wl,--end-group ${libxrdposix}
   ${BLAS_LIBRARIES} ${nsl} ${LAPACK_lapack_LIBRARY} ${crypt} ${MOTIF_LIBRARIES} ${Xbae} ${X11_Xaw_LIB} ${X11_Xt_LIB} ${X11_LIBRARIES} ${CMAKE_DL_LIBS})

endif()
target_include_directories(paw++.dynamic PRIVATE ${PROJECT_SOURCE_DIR}/include)
install(TARGETS paw++.dynamic DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
set_target_properties(paw++.dynamic PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME paw++.dynamic${COMPSUFFIXBIN})
endif()
add_executable(paw++ ${paw++_SRCS_F} )
if (APPLE)
  target_link_libraries(paw++ PRIVATE pawlib_static kernlib_static graflib_static ${grafX11}_static
   packlib_static paw_motif_static code_motif_static mathlib_static ${libxrdposix}
   ${BLAS_LIBRARIES} ${nsl} ${LAPACK_lapack_LIBRARY} ${crypt} ${MOTIF_LIBRARIES} ${Xbae} ${X11_Xaw_LIB} ${X11_Xt_LIB} ${X11_LIBRARIES} ${CMAKE_DL_LIBS})
else()
  target_link_libraries(paw++ PRIVATE -Wl,--start-group pawlib_static kernlib_static graflib_static ${grafX11}_static
   packlib_static paw_motif_static code_motif_static mathlib_static -Wl,--end-group ${libxrdposix}
   ${BLAS_LIBRARIES} ${nsl} ${LAPACK_lapack_LIBRARY} ${crypt} ${MOTIF_LIBRARIES} ${Xbae} ${X11_Xaw_LIB} ${X11_Xt_LIB} ${X11_LIBRARIES} ${CMAKE_DL_LIBS})
endif()
target_include_directories(paw++ PRIVATE ${PROJECT_SOURCE_DIR}/include)
install(TARGETS paw++ DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
set_target_properties(paw++ PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME paw++${COMPSUFFIXBIN})
########################################################################
set(zftp_SRCS_F geta.F getb.F getd.F getfz.F getp.F getrz.F getx.F
     mget.F mput.F puta.F putb.F putd.F putfz.F putp.F putrz.F
     putx.F rclose.F rlogin.F rshell.F zabend.F zexit.F zfctof.F
     zfftoc.F zffzcp.F zflogc.F zfrfrf.F zfrtof.F zfrzcp.F zftlcd.F
     zftlls.F zftlmv.F zftlrm.F zftpcd.F zftpls.F zftpmv.F
     zftprm.F zftpwd.F zfvers.F zftp.F)
     cdf_compile(${CMAKE_CURRENT_SOURCE_DIR}/packlib/cspack/programs/zftp/zftpcdf.cdf ${CMAKE_CURRENT_BINARY_DIR}/zftpcdf.cdf.c)
list(TRANSFORM zftp_SRCS_F PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/packlib/cspack/programs/zftp/")
add_executable(zftp ${zftp_SRCS_F} ${CMAKE_CURRENT_BINARY_DIR}/zftpcdf.cdf.c)
if (APPLE)
target_link_libraries(zftp PRIVATE   packlib_static kernlib_static graflib_static ${grafX11}_static
mathlib_static   ${X11_LIBRARIES} ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
else()
target_link_libraries(zftp PRIVATE -Wl,--start-group  packlib_static kernlib_static graflib_static ${grafX11}_static
mathlib_static -Wl,--end-group  ${X11_LIBRARIES} ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
endif()
target_include_directories(zftp PRIVATE ${PROJECT_SOURCE_DIR}/include)
install(TARGETS zftp DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
target_include_directories(zftp PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/cspack)
set_target_properties(zftp PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME zftp${COMPSUFFIXBIN})
########################################################################
set(fatback_SRCS_F fatback.F zabend.F)
list(TRANSFORM fatback_SRCS_F PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/packlib/fatmen/programs/fatback/")
add_executable(fatback  ${fatback_SRCS_F} )
if (APPLE)
  target_link_libraries(fatback PRIVATE packlib_static kernlib_static graflib_static ${grafX11}_static mathlib_static ${X11_LIBRARIES} ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
else()
  target_link_libraries(fatback PRIVATE packlib_static kernlib_static graflib_static ${grafX11}_static mathlib_static ${X11_LIBRARIES} ${BLAS_LIBRARIES} ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
endif()
target_include_directories(fatback PRIVATE ${PROJECT_SOURCE_DIR}/include)
install(TARGETS fatback DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
target_include_directories(fatback PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/fatmen)
set_target_properties(fatback PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME fatback${COMPSUFFIXBIN})
########################################################################
set(pawserv_SRCS_F zs.F fatmen.F message.F xzserv.F zscdir.F zsclos.F zsfile.F  zsldir.F zslfil.F zslist.F zsout.F zsrzin.F sgeta.F sgetfz.F  sgetp.F sgetrz.F sputa.F sputfz.F sputp.F sputrz.F)
if (CERNLIB_UNIX)
  list(APPEND pawserv_SRCS_F sppiaf.F ssfget.F )
endif()
if (CERNLIB_MMAP)
  list(APPEND pawserv_SRCS_F  zslism.F zslism1.F zslistm.F)
endif()
list(TRANSFORM pawserv_SRCS_F PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/packlib/cspack/programs/zs/")
add_executable(pawserv ${pawserv_SRCS_F} )
if (APPLE)
target_link_libraries(pawserv PRIVATE packlib_static pawlib_static kernlib_static graflib_static ${grafX11}_static mathlib_static ${X11_LIBRARIES} ${X11_Xaw_LIB} ${libxrdposix}
   ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} ${crypt})
else()
target_link_libraries(pawserv PRIVATE packlib_static pawlib_static kernlib_static graflib_static ${grafX11}_static mathlib_static ${X11_LIBRARIES} ${X11_Xaw_LIB} ${libxrdposix}
   ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} ${crypt})
endif()
target_include_directories(pawserv PRIVATE ${PROJECT_SOURCE_DIR}/include  )
install(TARGETS pawserv DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
target_include_directories(pawserv PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/cspack)
set_target_properties(pawserv PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME pawserv${COMPSUFFIXBIN})
#identical to pawserv
########################################################################
add_executable(zserv ${pawserv_SRCS_F} )
if (APPLE)
  target_link_libraries(zserv PRIVATE packlib_static pawlib_static kernlib_static graflib_static ${grafX11}_static mathlib_static ${X11_LIBRARIES} ${X11_Xaw_LIB} ${BLAS_LIBRARIES} ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
else()
  target_link_libraries(zserv PRIVATE packlib_static pawlib_static kernlib_static graflib_static ${grafX11}_static mathlib_static ${X11_LIBRARIES} ${X11_Xaw_LIB} ${BLAS_LIBRARIES} ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
endif()
target_include_directories(zserv PRIVATE ${PROJECT_SOURCE_DIR}/include  )
install(TARGETS zserv DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
target_include_directories(zserv PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/cspack)
target_include_directories(zserv PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/epio)
target_include_directories(zserv PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/fatmen)
target_include_directories(zserv PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/ffread)
target_include_directories(zserv PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/../mathlib/gen)
target_include_directories(zserv PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/kapack)
target_include_directories(zserv PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/kuip)
target_include_directories(zserv PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/minuit)
target_include_directories(zserv PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/zbook)
target_include_directories(zserv PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/zebra)
set_target_properties(zserv        PROPERTIES  POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME zserv${COMPSUFFIXBIN})
########################################################################
set(fatnew_SRCS_F fatnew.F)
list(TRANSFORM fatnew_SRCS_F PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/packlib/fatmen/programs/fatnew/")
add_executable(fatnew  ${fatnew_SRCS_F} )
if (APPLE)
  target_link_libraries(fatnew PRIVATE packlib_static kernlib_static graflib_static ${grafX11}_static mathlib_static
  ${X11_LIBRARIES} ${X11_Xaw_LIB} ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
else()
  target_link_libraries(fatnew PRIVATE packlib_static kernlib_static graflib_static ${grafX11}_static mathlib_static
  ${X11_LIBRARIES} ${X11_Xaw_LIB} ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
endif()
target_include_directories(fatnew PRIVATE ${PROJECT_SOURCE_DIR}/include  )
install(TARGETS fatnew DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
target_include_directories(fatnew PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/fatmen)
set_target_properties(fatnew PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME fatnew${COMPSUFFIXBIN})
########################################################################
set(fatmen_SRCS_F fatmen.F fmcd.F fmclr.F fmcopc.F fmcpc.F fmdumc.F
     fmedit.F fmexit.F fmextr.F fmfc.F fmfndc.F fmgime.F fminic.F
     fmkadd.F fmkadt.F fmkatt.F fmkcpl.F fmkdst.F fmkend.F fmklcc.F
     fmkloc.F fmkmod.F fmkmtp.F fmksrc.F fmkuwd.F fmkver.F
     fmld.F fmlnc.F
     fmlogc.F fmlogn.F fmlokk.F fmlsc.F fmmakc.F fmmkdr.F fmmvc.F
     fmnikk.F fmpwd.F fmquit.F fmrmc.F fmrmdc.F fmrmlc.F fmrmtc.F
     fmrzld.F fmscat.F fmscpl.F fmsdst.F fmseac.F fmskan.F fmslcc.F
     fmsloc.F fmsmdk.F fmsmtp.F fmssrc.F fmstat.F fmsupd.F fmsuwd.F
     fmsver.F fmtagc.F fmtchc.F fmthrg.F fmtrek.F fmulcc.F
     fmupdc.F fmzoom.F
     zabend.F)
cdf_compile(${CMAKE_CURRENT_SOURCE_DIR}/packlib/fatmen/programs/fmkuip/fmcdf.cdf ${CMAKE_CURRENT_BINARY_DIR}/fmcdf.cdf.c)
list(TRANSFORM fatmen_SRCS_F PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/packlib/fatmen/programs/fmkuip/")
add_executable(fatmen ${fatmen_SRCS_F} ${CMAKE_CURRENT_BINARY_DIR}/fmcdf.cdf.c)
if (APPLE)
target_link_libraries(fatmen PRIVATE  pawlib_static kernlib_static graflib_static ${grafX11}_static packlib_static
paw_motif_static
 code_motif_static mathlib_static
   ${BLAS_LIBRARIES} ${nsl} ${LAPACK_lapack_LIBRARY} ${crypt} ${MOTIF_LIBRARIES} ${Xbae} ${X11_Xaw_LIB} ${X11_Xt_LIB} ${X11_LIBRARIES} ${libxrdposix})
else()
target_link_libraries(fatmen PRIVATE -Wl,--start-group pawlib_static kernlib_static graflib_static ${grafX11}_static packlib_static
paw_motif_static
 code_motif_static mathlib_static -Wl,--end-group
   ${BLAS_LIBRARIES} ${nsl} ${LAPACK_lapack_LIBRARY} ${crypt} ${MOTIF_LIBRARIES} ${Xbae} ${X11_Xaw_LIB} ${X11_Xt_LIB} ${X11_LIBRARIES} ${libxrdposix})
endif()
target_include_directories(fatmen PRIVATE ${PROJECT_SOURCE_DIR}/include  )
install(TARGETS fatmen DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
target_include_directories(fatmen PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/fatmen)
set_target_properties(fatmen PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME fatmen${COMPSUFFIXBIN})
########################################################################
set(fatsend_SRCS_F fatsend.F)
list(TRANSFORM fatsend_SRCS_F PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/packlib/fatmen/programs/fatsend/")
add_executable(fatsend ${fatsend_SRCS_F} )
if (APPLE)
  target_link_libraries(fatsend PRIVATE packlib_static kernlib_static graflib_static ${grafX11}_static mathlib_static
  ${X11_LIBRARIES} ${X11_Xaw_LIB} ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
else()
  target_link_libraries(fatsend PRIVATE packlib_static kernlib_static graflib_static ${grafX11}_static mathlib_static
  ${X11_LIBRARIES} ${X11_Xaw_LIB} ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} ${libxrdposix} ${crypt})
endif()
target_include_directories(fatsend PRIVATE ${PROJECT_SOURCE_DIR}/include )
install(TARGETS fatsend DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
target_include_directories(fatsend PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/fatmen)
set_target_properties(fatsend PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME fatsend${COMPSUFFIXBIN})
########################################################################
set(fatsrv_SRCS_F abend.F facopy.F fatlog.F fatsrv.F fmallk.F fmfzi.F fmssnd.F)
list(TRANSFORM fatsrv_SRCS_F PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/packlib/fatmen/programs/fatsrv/")
add_executable(fatsrv ${fatsrv_SRCS_F})
if (APPLE)
  target_link_libraries(fatsrv PRIVATE packlib_static kernlib_static graflib_static ${grafX11}_static mathlib_static ${X11_LIBRARIES} ${X11_Xaw_LIB} ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} ${crypt} ${libxrdposix})
else()
  target_link_libraries(fatsrv PRIVATE packlib_static kernlib_static graflib_static ${grafX11}_static mathlib_static ${X11_LIBRARIES} ${X11_Xaw_LIB} ${BLAS_LIBRARIES}  ${LAPACK_lapack_LIBRARY} ${crypt} ${libxrdposix})
endif()
target_include_directories(fatsrv PRIVATE ${PROJECT_SOURCE_DIR}/include )
install(TARGETS fatsrv DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
target_include_directories(fatsrv PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/packlib/fatmen)
set_target_properties(fatsrv PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME fatsrv${COMPSUFFIXBIN})
########################################################################
#patchy4/p4lib/Imakefile
set(patchy4_p4lib_FSRC acexq.F auxini.F crdeco.F depart.F dospgo.F flink.F hollcv.F ictype.F iucomp.F jartyp.F kdfill.F
   locseq.F mqend.F msbyt.F pabend.F popin.F qfatal.F qsort.F qverif.F setnum.F ucopiv.F upkbyt.F vzero.F ybcdex.F yfrcex.F yshmac.F acpeel.F cbyt.F
   creapd.F depid.F doupd.F flkrak.F icdeci.F inchch.F iufila.F jbit.F kdnextvx.F locsq2.F mqgarb.F mxactd.F paberr.F popoff.F qfatam.F qsorth.F renamef.F
   setopt.F ucopy2.F upkch.F xinb.F ybinex.F yixex.F yshtag.F acseq.F cckrak.F crebuf.F depmsg.F douref.F floper.F icfila.F iofile.F iufind.F jbytet.F kdngo.F
   loglv.F mqinit.F mxactn.F pend.F poprew.F qlump.F qsorti.F rqtell.F tmpro.F ucopy.F uset.F xinbf.F ycomex.F ylisex.F yshval.F acsort.F ccproc.F cright.F doasm.F
   douse.F flpara.F icfind.F iofilx.F iufnbl.F jbyt.F kdnrew.F lqfind.F mqmove.F mxjoin.F pilect.F pready.F qlumpr.F qsorv.F sbit0.F toceta.F uctoh1.F utrans.F
   xinbs.F yedarr.F ylispg.F ytocex.F acxref.F cctoup.F csetdi.F dodeck.F dpexe.F frceta.F icfmul.F iotype.F iuforw.F jbytor.F kdstep.F lqlocf.F mqrelc.F mxnew.F
   pileup.F prtitl.F qname.F qsorvh.F sbit1.F tosetu.F uctoh.F vblank.F xincf.F yedkrk.F yserbf.F arbcd.F cetup.F csethi.F domatf.F dpexgo.F frsetu.F icfnbl.F
   iqbcd.F iulast.F jedchk.F kqlast.F lqlong.F mqtabl.F mxset.F pinit.F qabend.F qnamex.F qsorvi.F sbit.F traceq.F uh1toc.F vfill.F xoutb.F yedmat.F yserex.F
   arbin.F cfill.F csqmbl.F doopt.F dplist.F getenvf.F ichexi.F iqlocf.F iunext.F jedtyp.F liftbk.F mbytet.F msbit0.F namefl.F pkbyt.F qblow.F qpage.F qtchid.F
   sbyt.F ublank.F uhtoc.F vizpri.F xoutbf.F yedriv.F ysfind.F arrive.F cleft.F ctrans.F dorun.F dpmix.F getwdf.F icloc.F iucola.F iusame.F jrsbyt.F liftrh.F mbytor.F
   msbit1.F nomem.F pkchar.F qblowx.F qpostm.F qtopsy.F sbytor.F ublow.F uleft.F vxinvb.F xoutbs.F yedtex.F yshanl.F auxfil.F cltou.F cutol.F dospec.F dppage.F helppr.F
   icnext.F iucomh.F jartpx.F kdcopy.F lnblnk.F mcbyt.F msbit.F notitl.F pload.F qend.F qshunt.F qtouch.F setid.F ubunch.F uopt.F vxinvc.F xoutcf.F yedxch.F yshfex.F)
set(patchy4_p4lib_CSRC abend.c datime.c fchput.c fchtak.c geteni.c getwdi.c intrac.c ishft.c locb.c locf.c renami.c tminit.c tmproi.c tmread.c)
list(TRANSFORM patchy4_p4lib_FSRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/patchy4/p4lib/")
list(TRANSFORM patchy4_p4lib_CSRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/patchy4/p4lib/")
add_library(patchy4_static STATIC ${patchy4_p4lib_FSRC}  ${patchy4_p4lib_CSRC} )
target_include_directories(patchy4_static PRIVATE ${PROJECT_SOURCE_DIR}/include )
target_include_directories(patchy4_static PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/")
install(TARGETS patchy4_static DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT libs)
set_target_properties(patchy4_static PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME p4lib)
########################################################################
add_executable(ypatchy  ${CMAKE_CURRENT_SOURCE_DIR}/patchy4/patchy/ypatch.F  )
target_link_libraries(ypatchy PRIVATE patchy4_static)
target_include_directories(ypatchy PRIVATE ${PROJECT_SOURCE_DIR}/include)
install(TARGETS ypatchy DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
set_target_properties(ypatchy PROPERTIES OUTPUT_NAME ypatchy${COMPSUFFIXBIN})
#add_dependencies(ypatchy patchy4_static)
########################################################################
set(EXECS ycompa yedit yfrcet yindex ylist ysearc yshift ytobcd ytobin ytocet )
foreach ( XXX ${EXECS} )
   add_executable(${XXX}  ${CMAKE_CURRENT_SOURCE_DIR}/patchy4/patchy/${XXX}.F  )
   target_link_libraries(${XXX} PRIVATE patchy4_static)
   target_include_directories(${XXX} PRIVATE ${PROJECT_SOURCE_DIR}/include)
   install(TARGETS ${XXX} DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
   set_target_properties(${XXX} PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME ${XXX}${COMPSUFFIXBIN})
endforeach ( XXX ${EXECS} )

set_target_properties(ypatchy PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME ypatchy${COMPSUFFIXBIN})
set_target_properties(ycompa PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME ycompar${COMPSUFFIXBIN})
set_target_properties(yfrcet PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME yfrceta${COMPSUFFIXBIN})
set_target_properties(yindex PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME yindexb${COMPSUFFIXBIN})
set_target_properties(ylist  PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME ylistb${COMPSUFFIXBIN})
set_target_properties(ysearc PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME ysearch${COMPSUFFIXBIN})
set_target_properties(ytocet PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME ytoceta${COMPSUFFIXBIN})
########################################################################
file(MAKE_DIRECTORY  ${CMAKE_CURRENT_BINARY_DIR}/patchy5)
file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/patchy5/p5lib.cra DESTINATION  ${CMAKE_CURRENT_BINARY_DIR}/patchy5/)
add_custom_target( unsplit_SRC_p5libf  BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/patchy5/p5lib.f
#                        DEPENDS ypatchy
                        COMMAND      ${CMAKE_CURRENT_BINARY_DIR}/ypatchy${COMPSUFFIXBIN} - p5lib p5lib TTY .go
                       WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/patchy5)
add_dependencies(unsplit_SRC_p5libf ypatchy)


add_executable(fcasplit  ${CMAKE_CURRENT_SOURCE_DIR}/patchy5/fcasplit.F  )
install(TARGETS fcasplit DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
set_target_properties(fcasplit PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME fcasplit${COMPSUFFIXBIN} RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})


if (APPLE)
add_custom_target( SRC_p5libf ALL
                        COMMAND  ${CMAKE_CURRENT_BINARY_DIR}/fcasplit${COMPSUFFIXBIN} p5lib.f "-c "  "-c  -DCERNLIB_LINUX -DCERNLIB_UNIX -DCERNLIB_LNX -DCERNLIB_QMGLIBC -DCERNLIB_QMLXIA64  -DCERNLIB_VERBOSE -DCERNLIB_QGETCWD -DCERNLIB_GFORTRAN"   "" "gfortran" "gcc"
                       WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/patchy5 )
else()
add_custom_target( SRC_p5libf ALL
                        COMMAND  ${CMAKE_CURRENT_BINARY_DIR}/fcasplit${COMPSUFFIXBIN} p5lib.f "-c "  "-c  -DCERNLIB_LINUX -DCERNLIB_UNIX -DCERNLIB_LNX -DCERNLIB_QMGLIBC -DCERNLIB_QMLXIA64  -DCERNLIB_VERBOSE -DCERNLIB_QGETCWD -DCERNLIB_GFORTRAN"   "" "gfortran" "gcc"
                       WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/patchy5 )
endif()
add_dependencies(SRC_p5libf unsplit_SRC_p5libf fcasplit)
set (p5lib_SRC_F
   acsort.f   asmdump.f  ccjoin.f   dicrad.f    dif_tail.f  dinein.f  diwylr.f   doxqt.f   dplmsg.f   exlist.f    flkrak.f   iniaux.f  lideck.f   ln_get.f   lqfind.f  meload.f   mqgett.f  mqshift.f  na_dump.f  na_xco.f   pend.f     qblowx.f  shupto.f   smjopt.f   sylseq.f   tilist.f  x_next.f
   arrin.f    asmfind.f  cckrak.f   didupl.f    dif_xqt.f   dinewp.f  doapam.f   dpblan.f  dplmsq.f   exmerge.f   flname.f   inipam.f  liftsq.f   ln_mov2.f  lqstep.f  mesend.f   mqinit.f  mqshow.f   na_ful.f   nextcc.f   p_fatal.f  qlump.f   shwcomp.f  smrlog.f   sypam.f    tisend.f  x_opt.f
   arrive.f   asmini.f   ccopt.f    dif_back.f  dilink.f    dinoun.f  docrad.f   dpexe.f   dppage.f   exshell.f   flpara.f   iniseq.f  linksq.f   ln_put.f   m_a3ac.f  metitl.f   mqkill.f  mqtabl.f   na_get.f   nextde.f   p_fatam.f  qname.f   shwpar.f   smrxqt.f   sypat.f    tiseq.f   x_seq.f
   arrnxd.f   asmname.f  ccproc.f   dif_fmat.f  diload.f    diord.f   dodeck.f   dpexgo.f  dptag.f    exsynopt.f  flsplit.f  inisqx.f  linksu.f   ln_putr.f  m_a3if.f  m_fail.f   mqlift.f  mqwork.f   na_init.f  nextsi.f   pgsize.f   qnamex.f  shwsta.f   smsid.f    syseq.f    togarb.f  x_seqsp.f
   arrskp.f   asmuse.f   chdeck.f   dif_head.f  dilook.f    diordx.f  doforg.f   dpexmo.f  dqbank.f   extidy.f    helppr.f   iqlocf.f  lixqt.f    ln_to3.f   m_a3ke.f  m_head.f   mqlmov.f  m_split.f  na_inpy.f  nextxx.f   pinit2.f   qshlin.f  shwxqt.f   strmid.f   sysplit.f  togatx.f  x_udeck.f
   arrtm.f    cccoix.f   crdeco.f   dif_lx.f    dilost.f    diprem.f  domapa.f   dpextm.f  dqclobb.f  failcc.f    indcrd.f   jptype.f  ln_cop2.f  ln_to4.f   m_a3nx.f  mk_fnx.f   mqlrel.f  m_wnif.f   na_jtx.f   nospace.f  pinit.f    qshunt.f  smcil.f    sycrad.f   syxqt.f    usett.f   x_uput.f
   asmalia.f  cccomg.f   creact.f   dif_ly.f    dilread.f   disamd.f  domap.f    dphead.f  excheck.f  failln.f    indcrp.f   kqfind.f  ln_cop3.f  lockeep.f  m_a3sq.f  mk_frag.f  mqltab.f  mxjoin.f   na_long.f  nqcount.f  p_kill.f   qsorti.f  smcinst.f  syforg.f   ticomm.f   x_asm.f   x_use.f
   asmall.f   cccrak.f   creapd.f   dif_nx.f    dimname.f   disame.f  dopamco.f  dplact.f  exdiff.f   failmsg.f   indcrz.f   kqlast.f  ln_cop4.f  locpat.f   m_ana1.f  mk_nil.f   mqmove.f  mxoper.f   na_new.f   nsynopt.f  p_killm.f  qsortn.f  smcode.f   sykeep.f   tideck.f   xeject.f
   asmcrea.f  ccdopt.f   diacerr.f  dif_ny.f    dimstr.f    disend.f  dopam.f    dpline.f  exindex.f  flink.f     inddeck.f  kropt.f   ln_env3.f  locseq.f   m_ana2.f  m_krak.f   mqpage.f  na_chk.f   na_prix.f  pabend.f   psteer.f   qtopsy.f  smdef.f    syktag.f   tidistb.f  x_imit.f
   asmcrx.f   cciflv.f   diclfi.f   dif_same.f  dineed.f    diwylb.f  dopat.f    dplist.f  exitrc.f   fljoin.f    indxqt.f   krtitl.f  ln_equ.f   locsq2.f   m_ana3.f  mqgarb.f   mqrelc.f  na_dif.f   na_rein.f  p_crash.f  qblow.f    setgap.f  smdisp.f   sylkeep.f  tiforg.f   x_incl.f
   )
list(TRANSFORM p5lib_SRC_F PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/patchy5/p5boot/p5lib/")
add_library(p5lib_static STATIC ${p5lib_SRC_F})
add_dependencies(p5lib_static  SRC_p5libf)
set_target_properties(p5lib_static PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME p5lib  )

add_executable(nypatchy  ${CMAKE_CURRENT_SOURCE_DIR}/patchy5/p5boot/nypatchy.f  )
if (APPLE)
  target_link_libraries(nypatchy PRIVATE p5lib_static kernlib_static ${libxrdposix})
else()
  target_link_libraries(nypatchy PRIVATE p5lib_static kernlib_static ${libxrdposix})
endif()
target_include_directories(nypatchy PRIVATE ${PROJECT_SOURCE_DIR}/include)
install(TARGETS nypatchy DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
set_target_properties(nypatchy PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE} OUTPUT_NAME nypatchy${COMPSUFFIXBIN}  )

set( PRG nycheck nydiff nyindex nylist nymerge nyshell nysynopt nytidy )
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/patchy5/patchy.car   ${CMAKE_CURRENT_BINARY_DIR}/patchy5/patchy.car COPYONLY)
foreach ( XXX ${PRG} )
  add_custom_target( CRA_${XXX}  BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/patchy5/${XXX}.cra
                        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/patchy5/yexpand.script ${CMAKE_CURRENT_SOURCE_DIR}/patchy5/${XXX}.cra
                        COMMAND  ${CMAKE_COMMAND} -E env "PLINAME=${PLINAME}"     ${CMAKE_CURRENT_SOURCE_DIR}/patchy5/yexpand.script  ${CMAKE_CURRENT_SOURCE_DIR}/patchy5/${XXX}.cra ${XXX}.cra
                        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/patchy5/
                       )
  add_custom_target( SRC_${XXX}  BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/patchy5/${XXX}.f
                        DEPENDS ypatchy CRA_${XXX}
                        COMMAND      ${CMAKE_CURRENT_BINARY_DIR}/ypatchy${COMPSUFFIXBIN} - :${XXX}.f :${XXX}.cra  TTY .go
                       WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/patchy5/
                       )
  add_executable(${XXX}  ${CMAKE_CURRENT_BINARY_DIR}/patchy5/${XXX}.f  )
  add_dependencies(${XXX}  SRC_${XXX})
  install(TARGETS ${XXX} DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)
  set_target_properties(${XXX} PROPERTIES POSITION_INDEPENDENT_CODE ${CERNLIB_POSITION_INDEPENDENT_CODE}  OUTPUT_NAME ${XXX}${COMPSUFFIXBIN})
  target_link_libraries(${XXX} PRIVATE p5lib_static kernlib_static ${libxrdposix})
endforeach ( XXX ${PRG} )


include(CMakePackageConfigHelpers)

set(CONFIG_INSTALL_DIR ${CMAKE_INSTALL_DATADIR}/cernlib/cmake)

configure_package_config_file(cmake/Templates/CERNLIBConfig.cmake.in ${CMAKE_CURRENT_BINARY_DIR}/cmake/CERNLIBConfig.cmake
                              INSTALL_DESTINATION ${CONFIG_INSTALL_DIR}
                              PATH_VARS CMAKE_INSTALL_INCLUDEDIR
                              CMAKE_INSTALL_LIBDIR)

write_basic_package_version_file(${CMAKE_CURRENT_BINARY_DIR}/cmake/CERNLIBConfig-version.cmake COMPATIBILITY SameMajorVersion VERSION ${PROJECT_VERSION})


install(FILES ${CMAKE_CURRENT_BINARY_DIR}/cmake/CERNLIBConfig.cmake
              ${CMAKE_CURRENT_BINARY_DIR}/cmake/CERNLIBConfig-version.cmake
              DESTINATION ${CONFIG_INSTALL_DIR} COMPONENT devel)
