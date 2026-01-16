cmake_minimum_required(VERSION 3.12.0 FATAL_ERROR)
###########################Package@#####################################
project(SHERPA-MC LANGUAGES C  CXX)
#The complicated way to set the version is used to assure the simple
#replacement of version variable with sed will not break the CMakeLists.txt
set(EXTERNAL_VERSION_STRING "3.0.3")
string(REPLACE "."  ";" EXTERNAL_VERSION_LIST "${EXTERNAL_VERSION_STRING}")
list(GET EXTERNAL_VERSION_LIST 0 SHERPA_VERSION_MAJOR)
list(GET EXTERNAL_VERSION_LIST 1 SHERPA_VERSION_MINOR)
list(GET EXTERNAL_VERSION_LIST 2 SHERPA_VERSION_PATCH)
set(PROJECT_VERSION ${SHERPA_VERSION_MAJOR}.${SHERPA_VERSION_MINOR}.${SHERPA_VERSION_PATCH})
SET(SHERPA_VERSION ${SHERPA_VERSION_MAJOR}.${SHERPA_VERSION_MINOR}.${SHERPA_VERSION_PATCH})
# remove "alpha1", "beta2" etc.
string(REGEX REPLACE "[a-z]+[0-9]+" "" SHERPA_VERSION_PATCH_NUMBER
  "${SHERPA_VERSION_PATCH}")
math(EXPR SHERPA_VERSION_CODE "${SHERPA_VERSION_MAJOR} * 1000000 + ${SHERPA_VERSION_MINOR} * 1000 + ${SHERPA_VERSION_PATCH_NUMBER}")
message(STATUS "SHERPA: SHERPA_VERSION=${SHERPA_VERSION}")
message(STATUS "SHERPA: SHERPA_VERSION_MAJOR=${SHERPA_VERSION_MAJOR}")
message(STATUS "SHERPA: SHERPA_VERSION_MINOR=${SHERPA_VERSION_MINOR}")
message(STATUS "SHERPA: SHERPA_VERSION_PATCH=${SHERPA_VERSION_PATCH}")
message(STATUS "SHERPA: SHERPA_VERSION_PATCH_NUMBER=${SHERPA_VERSION_PATCH_NUMBER}")
message(STATUS "SHERPA: Building on CMAKE_SYSTEM_NAME=${CMAKE_SYSTEM_NAME}")
if(CMAKE_BUILD_TYPE)
  message(STATUS "SHERPA: Build type: ${CMAKE_BUILD_TYPE}")
else(CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "RelWithDebInfo")
  message(STATUS "SHERPA: Build type not specified, use: ${CMAKE_BUILD_TYPE}")
endif(CMAKE_BUILD_TYPE)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules ${CMAKE_MODULE_PATH} )
cmake_policy(SET CMP0069 NEW)

IF(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  SET(CMAKE_INSTALL_PREFIX ${PROJECT_BINARY_DIR} CACHE PATH "Default prefix" FORCE)
ENDIF(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
message(STATUS "SHERPA: CMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX}")
set(CMAKE_POSITION_INDEPENDENT_CODE TRUE)
###########################Options#####################################
option(SHERPA_ENABLE_ANALYSIS                "Enables building of Analyses." OFF)
option(SHERPA_ENABLE_BINRELOC                "Enables binreloc." OFF)
option(SHERPA_ENABLE_CATCH2                  "Enables Unit Tests with Catch2 (must be installed separately)" OFF)
option(SHERPA_ENABLE_CLANG_TIDY              "Enables checks with clang-tidy." OFF)
option(SHERPA_ENABLE_BLACKHAT                "Enables building with BlackHat." OFF)
option(SHERPA_ENABLE_DIHIGGS                 "Enables building of DIHIGGS." OFF)
option(SHERPA_ENABLE_DOXYGEN                 "Enables the 'docs' target for building HTML documentation" OFF)
option(SHERPA_ENABLE_EWSUD                   "Enables building of EWSUD." OFF)
option(SHERPA_ENABLE_EXAMPLES                "Enables installation of EXAMPLES." ON)
option(SHERPA_ENABLE_GOSAM                   "Enables building with GoSam." OFF)
option(SHERPA_ENABLE_GZIP                    "Enables building with GZIP." OFF)
option(SHERPA_ENABLE_HEPMC3                  "Enables building of HepMC3 interface." OFF)
option(SHERPA_ENABLE_HEPMC3_ROOT             "Enables building of HepMC3+ROOT interface ." OFF)
option(SHERPA_ENABLE_INTERNAL_PDFS           "Enables internal PDFs." ON)
option(SHERPA_ENABLE_INSTALL_LIBZIP          "Enables internal LibZip." OFF)
option(SHERPA_ENABLE_LHAPDF                  "Enables LHAPDF." ON)
option(SHERPA_ENABLE_INSTALL_LHAPDF          "Enables internal LHAPDF." OFF)
option(SHERPA_ENABLE_LHOLE                   "Enables building of LH OLE interface." OFF)
option(SHERPA_ENABLE_MADLOOP                 "Enables building with MadLoop." OFF)
option(SHERPA_ENABLE_MANUAL                  "Enables building of manual." OFF)
option(SHERPA_ENABLE_MCFM                    "Enables building with MCFM." OFF)
option(SHERPA_ENABLE_MPI                     "Enables building with MPI." OFF)
option(SHERPA_ENABLE_OPENLOOPS               "Enables building with OpenLoops." OFF)
option(SHERPA_ENABLE_PYTHIA8                 "Enables building with Pythia8." OFF)
option(SHERPA_ENABLE_PYTHON                  "Enables building of Python interface." OFF)
option(SHERPA_ENABLE_RECOLA                  "Enables building with Recola." OFF)
option(SHERPA_ENABLE_RIVET                   "Enables building with Rivet." OFF)
option(SHERPA_ENABLE_ROOT                    "Enables building with ROOT." OFF)
option(SHERPA_ENABLE_THREADING               "Enables building with threading." OFF)
option(SHERPA_ENABLE_TESTING                 "Enables testing." OFF)
option(SHERPA_ENABLE_UFO                     "Enables installation of UFO interfaces." OFF)
option(SHERPA_ENABLE_VERSIONING              "Enables versioned installation." OFF)


########################################################################
### allow the user to be sloppy and use HEPMC3_DIR instead of the correct
### capitalisation, HepMC3_DIR. Note that the capitalisation is defined
### by the HepMC3 package, not by us.
if (HEPMC3_DIR AND HepMC3_DIR)
  if (NOT "${HEPMC3_DIR}" STREQUAL "${HepMC3_DIR}")
    unset(HEPMC3_DIR CACHE)
    message(FATAL_ERROR "You have used both -DHEPMC3_DIR and -DHepMC3_DIR. Please only use -DHepMC3_DIR.")
  endif()
elseif (HEPMC3_DIR)
  message(WARNING "Please use -DHepMC3_DIR instead of -DHEPMC3_DIR. Will use the value found in HEPMC3_DIR to set HepMC3_DIR.")
  set(HepMC3_DIR "${HEPMC3_DIR}" CACHE FILEPATH "HepMC3 installation directory" FORCE)
endif()
unset(HEPMC3_DIR CACHE)
########################################################################
### enable option when path was provided explicitly via cmake
set(alldirenables BLACKHAT GOSAM HepMC3 LHAPDF MCFM OPENLOOPS PYTHIA8 RECOLA RIVET)
foreach (en IN LISTS alldirenables)
  if (${en}_DIR)
     string(TOUPPER "${en}" ENUP)
     SET(SHERPA_ENABLE_${ENUP} ON CACHE BOOL "" FORCE)
     message(STATUS "SHERPA: Set SHERPA_ENABLE_${ENUP}=ON as ${en}_DIR is given")
  endif()
endforeach()
###########################Options handeling############################
set( allenables ANALYSIS BINRELOC BLACKHAT CATCH2 CLANG_TIDY DIHIGGS DOXYGEN
 EWSUD EXAMPLES GOSAM GZIP HEPMC3 HEPMC3_ROOT
 INTERNAL_PDFS INSTALL_LIBZIP INSTALL_LHAPDF LHAPDF
 LHOLE MADLOOP MANUAL MCFM MPI OPENLOOPS PYTHIA8 PYTHON RECOLA RIVET ROOT THREADING TESTING UFO VERSIONING)
foreach (en IN LISTS allenables)
  message(STATUS "SHERPA: SHERPA_ENABLE_${en}=${SHERPA_ENABLE_${en}}")
endforeach()
if (SHERPA_ENABLE_INTERNAL_PDFS)
 set(SHERPA_ENABLE_FORTRAN ON)
 enable_language(Fortran)
 message(STATUS "SHERPA: SHERPA_ENABLE_FORTRAN=${SHERPA_ENABLE_FORTRAN}")
else()
 message(STATUS "SHERPA: SHERPA_ENABLE_FORTRAN=${SHERPA_ENABLE_FORTRAN}")
endif()
message(STATUS "SHERPA: CMAKE_CXX_STANDARD=${CMAKE_CXX_STANDARD}")
if (SHERPA_ENABLE_VERSIONING)
  set (VERSIONING "-${EXTERNAL_VERSION_STRING}")
else()
  set (VERSIONING "")
endif()
###########Respect HFS, e.g. defines lib or lib64 when it is needed.####
include("GNUInstallDirs")
if(CMAKE_SYSTEM_NAME MATCHES Darwin)
  if (NOT CMAKE_MACOSX_RPATH)
    set(CMAKE_MACOSX_RPATH 1)
  endif()
  message(STATUS "SHERPA: CMAKE_MACOSX_RPATH  ${CMAKE_MACOSX_RPATH}. Set by default to 1. Set to 0 to disable RPATH in the build executables.")
endif()
#### Modules ###########################################################
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules ${CMAKE_MODULE_PATH})
#### RPATH handling ####################################################
set(CMAKE_MACOSX_RPATH 1)
# use, i.e. don't skip the full RPATH for the build tree
SET(CMAKE_SKIP_BUILD_RPATH  FALSE)
# when building, don't use the install RPATH already (but later on when installing)
SET(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)
SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/SHERPA-MC${VERSIONING}")
# add the automatically determined parts of the RPATH
# which point to directories outside the build tree to the install RPATH
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
# the RPATH to be used when installing, but only if it's not a system directory
LIST(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/SHERPA-MC${VERSIONING}" isSystemDir)
IF("${isSystemDir}" STREQUAL "-1")
   SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/SHERPA-MC${VERSIONING}")
ENDIF("${isSystemDir}" STREQUAL "-1")
###################Windows##############################################
set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
###################FLAGS################################################
SET(CMAKE_POSITION_INDEPENDENT_CODE ${OLD_CMAKE_POSITION_INDEPENDENT_CODE})
if (SHERPA_ENABLE_FORTRAN)
  get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
  message(STATUS "SHERPA: Fortran_COMPILER_NAME=${Fortran_COMPILER_NAME}  CMAKE_Fortran_COMPILER_ID=${CMAKE_Fortran_COMPILER_ID}  CMAKE_Fortran_COMPILER_VERSION=${CMAKE_Fortran_COMPILER_VERSION}")
endif()
get_filename_component (C_COMPILER_NAME ${CMAKE_C_COMPILER} NAME)
message(STATUS "SHERPA: C_COMPILER_NAME=${C_COMPILER_NAME}  CMAKE_C_COMPILER_ID=${CMAKE_C_COMPILER_ID}   CMAKE_C_COMPILER_VERSION=${CMAKE_C_COMPILER_VERSION}")
get_filename_component (CXX_COMPILER_NAME ${CMAKE_CXX_COMPILER} NAME)
message(STATUS "SHERPA: CXX_COMPILER_NAME=${CXX_COMPILER_NAME}  CMAKE_CXX_COMPILER_ID=${CMAKE_CXX_COMPILER_ID}   CMAKE_CXX_COMPILER_VERSION=${CMAKE_CXX_COMPILER_VERSION}")

if(CMAKE_SYSTEM_NAME MATCHES Darwin)
  set(DARWIN_SHARED_LIBRARY_CREATE_FLAGS "-undefined dynamic_lookup")
  include(CheckLinkerFlagRegex)
  check_linker_flag_regex("-single_module" CXX "ld: warning: .* is obsolete" SINGLE_MODULE_FLAG_IS_NOT_OBSOLETE)
  if (SINGLE_MODULE_FLAG_IS_NOT_OBSOLETE)
    set(DARWIN_SHARED_LIBRARY_CREATE_FLAGS "-single_module ${DARWIN_SHARED_LIBRARY_CREATE_FLAGS}")
  endif()
  unset(SINGLE_MODULE_FLAG_IS_NOT_OBSOLETE)
  set(CMAKE_SHARED_LIBRARY_CREATE_Fortran_FLAGS "${CMAKE_SHARED_LIBRARY_CREATE_Fortran_FLAGS} ${DARWIN_SHARED_LIBRARY_CREATE_FLAGS}")
  set(CMAKE_SHARED_LIBRARY_CREATE_C_FLAGS "${CMAKE_SHARED_LIBRARY_CREATE_C_FLAGS} ${DARWIN_SHARED_LIBRARY_CREATE_FLAGS}")
  set(CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS "${CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS} ${DARWIN_SHARED_LIBRARY_CREATE_FLAGS}")
  unset(DARWIN_SHARED_LIBRARY_CREATE_FLAGS)
endif()

message(STATUS "SHERPA: CMAKE_EXE_LINKER_FLAGS=${CMAKE_EXE_LINKER_FLAGS}")
message(STATUS "SHERPA: CMAKE_MODULE_LINKER_FLAGS=${CMAKE_MODULE_LINKER_FLAGS}")
message(STATUS "SHERPA: CMAKE_SHARED_LINKER_FLAGS=${CMAKE_SHARED_LINKER_FLAGS}")
message(STATUS "SHERPA: CMAKE_STATIC_LINKER_FLAGS=${CMAKE_STATIC_LINKER_FLAGS}")
message(STATUS "SHERPA: CMAKE_EXE_LINKER_FLAGS_${CMAKE_BUILD_TYPE}=${CMAKE_EXE_LINKER_FLAGS_${CMAKE_BUILD_TYPE}}")
message(STATUS "SHERPA: CMAKE_MODULE_LINKER_FLAGS_${CMAKE_BUILD_TYPE}=${CMAKE_MODULE_LINKER_FLAGS_${CMAKE_BUILD_TYPE}}")
message(STATUS "SHERPA: CMAKE_SHARED_LINKER_FLAGS_${CMAKE_BUILD_TYPE}=${CMAKE_SHARED_LINKER_FLAGS_${CMAKE_BUILD_TYPE}}")
message(STATUS "SHERPA: CMAKE_STATIC_LINKER_FLAGS_${CMAKE_BUILD_TYPE}=${CMAKE_STATIC_LINKER_FLAGS_${CMAKE_BUILD_TYPE}}")
message(STATUS "SHERPA: CMAKE_Fortran_FLAGS_${CMAKE_BUILD_TYPE}=${CMAKE_Fortran_FLAGS_${CMAKE_BUILD_TYPE}}")
message(STATUS "SHERPA: CMAKE_C_FLAGS_${CMAKE_BUILD_TYPE}=${CMAKE_C_FLAGS_${CMAKE_BUILD_TYPE}}")
message(STATUS "SHERPA: CMAKE_CXX_FLAGS_${CMAKE_BUILD_TYPE}=${CMAKE_CXX_FLAGS_${CMAKE_BUILD_TYPE}}")

include(CheckCCompilerFlag)
include(CheckCXXCompilerFlag)
set(CXX_CXX_FLAGS_TO_CHECK -Wno-tautological-constant-compare)
set ( NEW_CMAKE_CXX_FLAGS " ")
string(REPLACE " -" ";-" LIST_CMAKE_CXX_FLAGS "${CXX_CXX_FLAGS_TO_CHECK} ${CMAKE_CXX_FLAGS}")
foreach(fx IN LISTS LIST_CMAKE_CXX_FLAGS)
  if (fx)
    check_cxx_compiler_flag(${fx} CXX_COMPILER_SUPPORTS_${fx})
    if(CXX_COMPILER_SUPPORTS_${fx})
      list( APPEND NEW_CMAKE_CXX_FLAGS ${fx})
      set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${fx}" ) #neede for dependent flags
    endif()
  endif()
endforeach()
list(JOIN NEW_CMAKE_CXX_FLAGS " " CMAKE_CXX_FLAGS_X )
if (NOT CMAKE_FORCE_FLAGS)
  set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS_X})
endif()
message(STATUS "SHERPA: CMAKE_CXX_FLAGS=${CMAKE_CXX_FLAGS}")
########################################################################

set ( NEW_CMAKE_C_FLAGS " ")
string(REPLACE " " ";" LIST_CMAKE_C_FLAGS "${C_C_FLAGS_TO_CHECK} ${CMAKE_C_FLAGS}")
foreach(fx IN LISTS LIST_CMAKE_C_FLAGS)
  if (fx)
    check_c_compiler_flag(${fx} C_COMPILER_SUPPORTS_${fx})
    if(C_COMPILER_SUPPORTS_${fx})
      list( APPEND NEW_CMAKE_C_FLAGS ${fx})
      set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${fx}" ) #neede for dependent flags
    endif()
  endif()
endforeach()
list(JOIN NEW_CMAKE_C_FLAGS " " CMAKE_C_FLAGS_X )
if (NOT CMAKE_FORCE_FLAGS)
  set(CMAKE_C_FLAGS ${CMAKE_C_FLAGS_X})
endif()
message(STATUS "SHERPA: CMAKE_C_FLAGS=${CMAKE_C_FLAGS}")
########################################################################
if (SHERPA_ENABLE_FORTRAN)
  include(CheckFortranCompilerFlag)
  set ( NEW_CMAKE_Fortran_FLAGS " ")
  string(REPLACE " " ";" LIST_CMAKE_Fortran_FLAGS  "${F_Fortran_FLAGS_TO_CHECK} ${CMAKE_Fortran_FLAGS}")
  foreach(fl IN LISTS LIST_CMAKE_Fortran_FLAGS)
    if (fl)
      check_Fortran_compiler_flag(${fl} Fortran_COMPILER_SUPPORTS_${fl})
      if(Fortran_COMPILER_SUPPORTS_${fl})
        list( APPEND NEW_CMAKE_Fortran_FLAGS ${fl})
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${fl}" ) #neede for dependent flags
      endif()
    endif()
  endforeach()
  list(JOIN NEW_CMAKE_Fortran_FLAGS " " CMAKE_Fortran_FLAGS_X )
  if (NOT CMAKE_FORCE_FLAGS)
    set(CMAKE_Fortran_FLAGS ${CMAKE_Fortran_FLAGS_X})
  endif()
  message(STATUS "SHERPA: CMAKE_Fortran_FLAGS=${CMAKE_Fortran_FLAGS}")
endif()
########################################################################
if (SHERPA_ENABLE_CLANG_TIDY)
  set(CMAKE_CXX_CLANG_TIDY "clang-tidy;--checks=*,-readability-identifier-length,-modernize-use-trailing-return-type,-cppcoreguidelines-avoid-non-const-global-variables,-llvmlibc-callee-namespace,-bugprone-easily-swappable-parameters,-cppcoreguidelines-pro-bounds-pointer-arithmetic,-readability-implicit-bool-conversion,-hicpp-braces-around-statements,-readability-braces-around-statements,-cert-err34-c,-modernize-return-braced-init-list")
endif()
########################################################################

find_program(GIT NAMES git)
if (GIT AND EXISTS "${CMAKE_SOURCE_DIR}/.git")
  execute_process(COMMAND ${GIT} rev-parse --abbrev-ref HEAD
                  WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
                  OUTPUT_VARIABLE GITURL
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND ${GIT} rev-parse  HEAD
                  WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
                  OUTPUT_VARIABLE GITREV
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  message(STATUS "SHERPA: GIT=${GIT} GITREV=${GITREV} GITURL=${GITURL}")
else()
  message(STATUS "SHERPA: GIT IS NOT AVAILABLE!")
  set(GITURL "unknownurl")
  set(GITREV "unknownrevision")
endif()

define_property(
  GLOBAL PROPERTY ALL_SOURCE_FILES
  BRIEF_DOCS "Collects all the source files from subdirectories"
  FULL_DOCS "Source files found by the sherpa_create_git_info macro go here"
)
macro(sherpa_create_git_info name)
  if (GIT AND EXISTS "${CMAKE_SOURCE_DIR}/.git")
    execute_process(COMMAND ${GIT} status -s --untracked-files=no .
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                  OUTPUT_VARIABLE GITCLEAN
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  else()
   set(GITCLEAN "")
  endif()
  if ("${GITCLEAN}" STREQUAL "")
    set(GITREVSUFFIX "")
  else()
    set(GITREVSUFFIX "-dirty")
#Message only changes
    message(STATUS "SHERPA ${CMAKE_CURRENT_SOURCE_DIR}: GITCLEAN=${GITCLEAN} GITREVSUFFIX=${GITREVSUFFIX}")
  endif()
  set(allsources )
  foreach (f IN LISTS ${name}_SOURCES  ${name}_HEADERS )
    list(APPEND allsources ${f})
  endforeach()
  list (FILTER allsources EXCLUDE REGEX ".*CXXFLAGS.*")
  set(content "")
  foreach (f IN LISTS allsources )
    file(READ "${CMAKE_CURRENT_SOURCE_DIR}/${f}"  temp)
    string(APPEND content "${temp}")
  endforeach()
  string(REPLACE "${PROJECT_SOURCE_DIR}/" "" GITTAG "${CMAKE_CURRENT_SOURCE_DIR}")
  string(MD5 MDF "${content}")
  set(newgitinfo "#include \"ATOOLS/Org/Git_Info.H\"\nstatic ATOOLS::Git_Info initializer\n(\"${GITTAG}\",\"${GITURL}\",\"${GITREV}${GITREVSUFFIX}\",\"${MDF}\");\n")
  set(oldgitinfo "")
  if (EXISTS ${CMAKE_CURRENT_BINARY_DIR}/Git_Info.C)
    file(READ ${CMAKE_CURRENT_BINARY_DIR}/Git_Info.C oldgitinfo)
  endif()
  if ( NOT "${newgitinfo}" STREQUAL "${oldgitinfo}")
    file( WRITE ${CMAKE_CURRENT_BINARY_DIR}/Git_Info.C "${newgitinfo}")
  endif()
  list(TRANSFORM allsources PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/")
  set_property(GLOBAL APPEND PROPERTY ALL_SOURCE_FILES ${allsources})
endmacro(sherpa_create_git_info )

if(SHERPA_ENABLE_MPI)
  find_package(MPI REQUIRED)
  message(STATUS "SHERPA: MPIEXEC_EXECUTABLE=${MPIEXEC_EXECUTABLE}")
  if (SHERPA_ENABLE_FORTRAN)
    message(STATUS "SHERPA: MPI_CXX_COMPILE_OPTIONS=${MPI_CXX_COMPILE_OPTIONS} MPI_C_COMPILE_OPTIONS=${MPI_C_COMPILE_OPTIONS} MPI_Fortran_COMPILE_OPTIONS=${MPI_Fortran_COMPILE_OPTIONS}")
    message(STATUS "SHERPA: MPI_CXX_COMPILE_DEFINITIONS=${MPI_CXX_COMPILE_DEFINITIONS} MPI_C_COMPILE_DEFINITIONS=${MPI_C_COMPILE_DEFINITIONS} MPI_Fortran_COMPILE_DEFINITIONS=${MPI_Fortran_COMPILE_DEFINITIONS}")
    message(STATUS "SHERPA: MPI_CXX_INCLUDE_DIRS=${MPI_CXX_INCLUDE_DIRS} MPI_C_INCLUDE_DIRS=${MPI_C_INCLUDE_DIRS} MPI_Fortran_INCLUDE_DIRS=${MPI_Fortran_INCLUDE_DIRS}")
    message(STATUS "SHERPA: MPI_CXX_LINK_FLAGS=${MPI_CXX_LINK_FLAGS} MPI_C_LINK_FLAGS=${MPI_C_LINK_FLAGS} MPI_Fortran_LINK_FLAGS=${MPI_Fortran_LINK_FLAGS}")
  else()
    message(STATUS "SHERPA: MPI_CXX_COMPILE_OPTIONS=${MPI_CXX_COMPILE_OPTIONS} MPI_C_COMPILE_OPTIONS=${MPI_C_COMPILE_OPTIONS}")
    message(STATUS "SHERPA: MPI_CXX_COMPILE_DEFINITIONS=${MPI_CXX_COMPILE_DEFINITIONS} MPI_C_COMPILE_DEFINITIONS=${MPI_C_COMPILE_DEFINITIONS}")
    message(STATUS "SHERPA: MPI_CXX_INCLUDE_DIRS=${MPI_CXX_INCLUDE_DIRS} MPI_C_INCLUDE_DIRS=${MPI_C_INCLUDE_DIRS}")
    message(STATUS "SHERPA: MPI_CXX_LINK_FLAGS=${MPI_CXX_LINK_FLAGS} MPI_C_LINK_FLAGS=${MPI_C_LINK_FLAGS}")
  endif()
  set (LOADMPIFORPY "from mpi4py import MPI")
else()
  set (LOADMPIFORPY "")
endif()
if (SHERPA_ENABLE_THREADING)
  set(THREADS_PREFER_PTHREAD_FLAG ON)
  find_package(Threads REQUIRED)
endif()

macro(sherpa_handle_shared_library  mylib mylibname)
  if(SHERPA_ENABLE_MPI)
     target_link_libraries(${mylib} PRIVATE MPI::MPI_CXX)
     if (SHERPA_ENABLE_FORTRAN)
       target_link_libraries(${mylib} PRIVATE MPI::MPI_Fortran)
     endif()
  endif()
  target_include_directories(${mylib} PUBLIC
    $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}>
    $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/SHERPA-MC${VERSIONING}>)
  install(TARGETS ${mylib} EXPORT SHERPATargets DESTINATION ${CMAKE_INSTALL_LIBDIR}/SHERPA-MC${VERSIONING} COMPONENT libs)
  set_target_properties(${mylib}        PROPERTIES POSITION_INDEPENDENT_CODE ON  SOVERSION 0.0.0)
  set_target_properties(${mylib}        PROPERTIES DEFINE_SYMBOL "")
  set_target_properties(${mylib}        PROPERTIES OUTPUT_NAME ${mylibname})
  target_link_libraries(${mylib}  PRIVATE Threads::Threads)
  target_compile_features(${mylib} PRIVATE cxx_std_11)
  set_target_properties(${mylib} PROPERTIES  ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/outputs/${CMAKE_INSTALL_LIBDIR}/SHERPA-MC/$<0:>
                                             LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/outputs/${CMAKE_INSTALL_LIBDIR}/SHERPA-MC/$<0:>
                                             RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/outputs/${CMAKE_INSTALL_LIBDIR}/SHERPA-MC/$<0:>
                                             EXPORT_NAME SHERPA::${mylib}
                                               )

endmacro(sherpa_handle_shared_library  mylib mylibname)

macro(sherpa_handle_executable  mylib )
  if(SHERPA_ENABLE_MPI)
     target_link_libraries(${mylib} PRIVATE MPI::MPI_CXX)
     if (SHERPA_ENABLE_FORTRAN)
       target_link_libraries(${mylib} PRIVATE MPI::MPI_Fortran)
     endif()
  endif()
  set_target_properties(${mylib} PROPERTIES  ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/outputs/${CMAKE_INSTALL_BINDIR}/$<0:>
                                             LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/outputs/${CMAKE_INSTALL_BINDIR}/$<0:>
                                             RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/outputs/${CMAKE_INSTALL_BINDIR}/$<0:>
                                               )
   if(NOT (CMAKE_SYSTEM_NAME MATCHES Darwin))
     target_link_options(${mylib} PRIVATE "LINKER:-no-as-needed")
   endif()
   target_compile_features(${mylib} PRIVATE cxx_std_11)
endmacro(sherpa_handle_executable  mylib )


if (NOT SHERPA_ENABLE_INSTALL_LIBZIP)
  find_package(LibZip)
  if (NOT LibZip_FOUND)
    message(FATAL_ERROR "SHERPA: LibZip not found! Terminating!\n To use installed LibZip add to the cmake configuration -DLibZip_DIR=/installation/path/to/libzip or use -DSHERPA_ENABLE_INSTALL_LIBZIP=ON to download and compile LibZip")
  endif()
else()
  message(STATUS "SHERPA: LibZip will be downloaded and compiled")
  include(ExternalProject)
  include(FetchContent)
  ExternalProject_Add(downloadedlibzip
    GIT_REPOSITORY https://github.com/nih-at/libzip
    GIT_TAG v1.9.2
    STEP_TARGETS build
    GIT_SHALLOW 1
    UPDATE_COMMAND ""
    PATCH_COMMAND ""
    PREFIX ${PROJECT_BINARY_DIR}/EXTERNALSRC
    CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${PROJECT_BINARY_DIR}/EXTERNALINSTALL/libzip -DCMAKE_INSTALL_LIBDIR=lib -DCMAKE_CXX_FLAGS=${CMAKE_CXX_FLAGS} -DBUILD_DOC=OFF -DBUILD_EXAMPLES=OFF -DBUILD_TOOLS=OFF -DBUILD_REGRESS=OFF
    BUILD_BYPRODUCTS ${PROJECT_BINARY_DIR}/EXTERNALINSTALL/libzip/lib/libzip${CMAKE_SHARED_LIBRARY_SUFFIX}
  )
  unset(LibZip_ROOT)
  set(LibZip_DIR ${PROJECT_BINARY_DIR}/EXTERNALINSTALL/libzip)
  set(LibZip_VERSION 1.9.2)
  set(LibZip_LIBRARIES ${LibZip_DIR}/lib/libzip${CMAKE_SHARED_LIBRARY_SUFFIX})
  set(LibZip_INCLUDE_DIRS ${LibZip_DIR}/include)
  file(MAKE_DIRECTORY ${LibZip_DIR}/include)
  add_library(LibZip::LibZip UNKNOWN IMPORTED)
  set_target_properties(LibZip::LibZip PROPERTIES
        IMPORTED_LOCATION "${LibZip_LIBRARIES}"
        INTERFACE_INCLUDE_DIRECTORIES "${LibZip_INCLUDE_DIRS}"
    )
  if (CMAKE_SYSTEM_NAME MATCHES Darwin)
    install(FILES ${LibZip_DIR}/lib/libzip.5.5${CMAKE_SHARED_LIBRARY_SUFFIX} DESTINATION ${CMAKE_INSTALL_LIBDIR}/SHERPA-MC${VERSIONING} COMPONENT libs)
    install(FILES ${LibZip_DIR}/lib/libzip.5${CMAKE_SHARED_LIBRARY_SUFFIX} DESTINATION ${CMAKE_INSTALL_LIBDIR}/SHERPA-MC${VERSIONING} COMPONENT libs)
  else()
    install(FILES ${LibZip_LIBRARIES}.5.5 DESTINATION ${CMAKE_INSTALL_LIBDIR}/SHERPA-MC${VERSIONING} COMPONENT libs)
    install(FILES ${LibZip_LIBRARIES}.5 DESTINATION ${CMAKE_INSTALL_LIBDIR}/SHERPA-MC${VERSIONING} COMPONENT libs)
  endif()
  install(FILES ${LibZip_LIBRARIES} DESTINATION ${CMAKE_INSTALL_LIBDIR}/SHERPA-MC${VERSIONING} COMPONENT libs)
  install(FILES ${LibZip_INCLUDE_DIRS}/zip.h DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/SHERPA-MC${VERSIONING} COMPONENT libs)
  add_dependencies(LibZip::LibZip PRIVATE downloadedlibzip)
endif()
message(STATUS "SHERPA: LibZip_VERSION=${LibZip_VERSION} LibZip_LIBRARIES=${LibZip_LIBRARIES} LibZip_INCLUDE_DIR=${LibZip_INCLUDE_DIR}")

if (SHERPA_ENABLE_LHAPDF)
  if(NOT SHERPA_ENABLE_INSTALL_LHAPDF)
    find_package(LHAPDF 6.4.0)
    if (NOT LHAPDF_FOUND)
      message(FATAL_ERROR "SHERPA: LHAPDF not found! Terminating!\n To use installed LHAPDF add to the cmake configuration -DLHAPDF_DIR=/installation/path/to/lhapdf or use -DSHERPA_ENABLE_INSTALL_LHAPDF=ON to download and compile LHAPDF")
    endif()
  else()
    message(STATUS "SHERPA: LHAPDF will be downloaded and compiled")
    include(ExternalProject)
    include(FetchContent)
    set(LHAPDFDESTDIR ${PROJECT_BINARY_DIR}/EXTERNALINSTALL/lhapdf)
    ExternalProject_Add(downloadedlhapdf
      URL https://lhapdf.hepforge.org/downloads/?f=LHAPDF-6.5.3.tar.gz
      UPDATE_COMMAND ""
      #LHAPDF is  bugged
      #One should not have doxygen in the path!
      #PATCH_COMMAND  "sed -i 's/ac_cv_path_DOXYGEN/DOXYGEN/g' configure"
      BUILD_IN_SOURCE     1
      PREFIX ${PROJECT_BINARY_DIR}/EXTERNALSRC
      CONFIGURE_COMMAND ./configure --disable-python --without-doxygen --prefix=${CMAKE_INSTALL_PREFIX} --libdir=${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/SHERPA-MC${VERSIONING} --datadir=${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING}
      BUILD_COMMAND make -j 8
      INSTALL_COMMAND make install DESTDIR=${LHAPDFDESTDIR}
      BUILD_BYPRODUCTS
                     ${LHAPDFDESTDIR}/${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/SHERPA-MC${VERSIONING}/libLHAPDF${CMAKE_SHARED_LIBRARY_SUFFIX}
                     ${LHAPDFDESTDIR}/${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING}/LHAPDF/lhapdf.conf
                     ${LHAPDFDESTDIR}/${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING}/LHAPDF/pdfsets.index
    )
    unset(LHAPDF_ROOT)
    set(LHAPDF_DIR ${LHAPDFDESTDIR}/${CMAKE_INSTALL_PREFIX})
    set(LHAPDF_PATH ${CMAKE_INSTALL_PREFIX})
    set(LHAPDF_DATA_PATH ${LHAPDFDESTDIR}/${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING}/LHAPDF/)
    set(LHAPDF_VERSION 6.5.3)
    set(LHAPDF_LIBRARIES ${LHAPDFDESTDIR}/${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/SHERPA-MC${VERSIONING}/libLHAPDF${CMAKE_SHARED_LIBRARY_SUFFIX})
    set(LHAPDF_LIBRARY_DIR ${LHAPDFDESTDIR}/${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/SHERPA-MC${VERSIONING})
    set(LHAPDF_INCLUDE_DIRS ${LHAPDF_DIR}/include)
    file(MAKE_DIRECTORY ${LHAPDF_DIR}/include)
    add_library(LHAPDF::LHAPDF UNKNOWN IMPORTED)
    set_target_properties(LHAPDF::LHAPDF PROPERTIES
          IMPORTED_LOCATION "${LHAPDF_LIBRARIES}"
          INTERFACE_INCLUDE_DIRECTORIES "${LHAPDF_INCLUDE_DIRS}"
      )
    install(FILES
                  ${LHAPDFDESTDIR}/${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING}/LHAPDF/lhapdf.conf
                  ${LHAPDFDESTDIR}/${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING}/LHAPDF/pdfsets.index
                  DESTINATION ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING}/ COMPONENT libs)
    install(FILES ${LHAPDF_LIBRARIES} DESTINATION ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/SHERPA-MC${VERSIONING} COMPONENT libs)
    install(DIRECTORY ${LHAPDF_INCLUDE_DIRS}/LHAPDF DESTINATION ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_INCLUDEDIR}/SHERPA-MC${VERSIONING} COMPONENT libs)
    install(DIRECTORY ${LHAPDF_DIR}/share/SHERPA-MC${VERSIONING}/LHAPDF  DESTINATION ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING} COMPONENT libs)
    add_dependencies(LHAPDF::LHAPDF PRIVATE downloadedlhapdf)
    file(MAKE_DIRECTORY ${LHAPDFDESTDIR}/${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING}/LHAPDF/)
    file(DOWNLOAD https://www.hep.ucl.ac.uk/pdf4lhc/pdf4lhc21grids/PDF4LHC21_40_pdfas.tgz ${LHAPDFDESTDIR}/${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING}/LHAPDF//PDF4LHC21_40_pdfas.tgz)
    add_custom_target( unZipPDF4LHC21_40_pdfas ALL)
    add_custom_command(TARGET unZipPDF4LHC21_40_pdfas PRE_BUILD
         COMMAND ${CMAKE_COMMAND} -E remove_directory ${LHAPDFDESTDIR}/${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING}/LHAPDF//PDF4LHC21_40_pdfas
         COMMAND ${CMAKE_COMMAND} -E tar xzf ${LHAPDFDESTDIR}/${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING}/LHAPDF/PDF4LHC21_40_pdfas.tgz
         WORKING_DIRECTORY ${LHAPDFDESTDIR}/${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING}/LHAPDF/
         DEPENDS ${LHAPDFDESTDIR}/${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING}/LHAPDF//PDF4LHC21_40_pdfas.tar.bz2
         COMMENT "Unpacking PDF4LHC21_40_pdfas.tar.bz2"
         VERBATIM)
    install(DIRECTORY ${LHAPDFDESTDIR}/${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING}/LHAPDF//PDF4LHC21_40_pdfas DESTINATION  ${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING}/LHAPDF/ COMPONENT doc )
  endif()
else()
  set(SHERPA_ENABLE_INSTALL_LHAPDF OFF)
endif()

message(STATUS "SHERPA: LHAPDF_VERSION=${LHAPDF_VERSION} LHAPDF_LIBRARIES=${LHAPDF_LIBRARIES} LHAPDF_INCLUDE_DIRS=${LHAPDF_INCLUDE_DIRS}")

add_subdirectory(AHADIC++)
add_subdirectory(AMEGIC++)
add_subdirectory(AMISIC++)
add_subdirectory(BEAM)
add_subdirectory(COMIX)
add_subdirectory(CSSHOWER++)
add_subdirectory(DIM)
add_subdirectory(DIRE)
add_subdirectory(EXTAMP)
add_subdirectory(EXTRA_XS)
add_subdirectory(HADRONS++)
add_subdirectory(METOOLS)
add_subdirectory(MCATNLO)
add_subdirectory(MODEL)
add_subdirectory(PHASIC++)
add_subdirectory(PHOTONS++)
add_subdirectory(PDF/Electron)
if (SHERPA_ENABLE_INTERNAL_PDFS)
  add_subdirectory(PDF/GRV)
  add_subdirectory(PDF/CJK)
  add_subdirectory(PDF/CT14)
  add_subdirectory(PDF/SASG)
endif()
if(SHERPA_ENABLE_LHAPDF)
  add_subdirectory(PDF/LHAPDF)
endif()
add_subdirectory(ATOOLS)
add_subdirectory(PDF/Main)
add_subdirectory(PDF/NNPDF)
add_subdirectory(PDF/SAL)
add_subdirectory(RECONNECTIONS)
add_subdirectory(REMNANTS)
add_subdirectory(SHRiMPS)
add_subdirectory(YFS)
IF (SHERPA_ENABLE_MCFM)
  find_package(MCFM NAMES mcfm QUIET)
  if (NOT MCFM_FOUND)
    include(FindMCFM)
  endif()
  message(STATUS "SHERPA: MCFM_VERSION=${MCFM_VERSION} MCFM_LIBRARY=${MCFM_LIBRARY} MCFM_INCLUDE_DIR=${MCFM_INCLUDE_DIR}")
  add_subdirectory(AddOns/MCFM)
endif()

if (SHERPA_ENABLE_RIVET)
  find_package(Rivet 3.0.0 REQUIRED)
  message(STATUS "SHERPA: RIVET_VERSION=${RIVET_VERSION} RIVET_FOUND=${RIVET_FOUND}  RIVET_LIBRARIES=${RIVET_LIBRARIES} \
  RIVET_INCLUDE_DIRS=${RIVET_INCLUDE_DIRS} RIVET_DATA_PATH=${RIVET_DATA_PATH} RIVET_ANALYSIS_PATH=${RIVET_ANALYSIS_PATH} \
  RIVET_CONFIG_LIBS_STRING=${RIVET_CONFIG_LIBS_STRING}  RIVET_CONFIG_LIBS=${RIVET_CONFIG_LIBS}  RIVET_CONFIG_LIB_DIRS=${RIVET_CONFIG_LIB_DIRS} \
  RIVET_CONFIG_CPPFLAGS_STRING=${RIVET_CONFIG_CPPFLAGS_STRING}  RIVET_CONFIG_CPPFLAGS_DIRS=${RIVET_CONFIG_CPPFLAGS_DIRS}")
  message(STATUS "SHERPA: WARNING: HepMC3 with RIVET_HEPMC3_MIN_VERSION=${RIVET_HEPMC3_MIN_VERSION} will be requested by Rivet.")
  message(STATUS "SHERPA: YODA_VERSION=${YODA_VERSION} YODA_LIBRARIES=${YODA_LIBRARIES} YODA_INCLUDE_DIRS=${YODA_INCLUDE_DIRS} YODA_CONFIG_PYTHONPATH_STRING=${YODA_CONFIG_PYTHONPATH_STRING}")
endif()
if (SHERPA_ENABLE_HEPMC3 OR SHERPA_ENABLE_RIVET)
  if (SHERPA_ENABLE_HEPMC3_ROOT)
    find_package(HepMC3 ${RIVET_HEPMC3_MIN_VERSION} REQUIRED COMPONENTS search rootIO CONFIG HINTS ${HepMC3_DIR})
  else()
    find_package(HepMC3 ${RIVET_HEPMC3_MIN_VERSION} REQUIRED COMPONENTS search CONFIG HINTS ${HepMC3_DIR})
  endif()
  if (NOT TARGET HepMC3::HepMC3 AND HEPMC3_LIB)
    add_library(HepMC3::HepMC3 UNKNOWN IMPORTED)
    set_target_properties(HepMC3::HepMC3 PROPERTIES IMPORTED_LOCATION "${HEPMC3_LIB}" INTERFACE_INCLUDE_DIRECTORIES "${HEPMC3_INCLUDE_DIR}")
  endif()
  if (NOT TARGET HepMC3::rootIO AND HEPMC3_ROOTIO_LIB)
    add_library(HepMC3::rootIO UNKNOWN IMPORTED)
    set_target_properties(HepMC3::rootIO PROPERTIES IMPORTED_LOCATION "${HEPMC3_ROOTIO_LIB}" INTERFACE_INCLUDE_DIRECTORIES "${HEPMC3_INCLUDE_DIR}")
  endif()
  if (NOT TARGET HepMC3::search AND HEPMC3_SEARCH_LIB)
    add_library(HepMC3::search UNKNOWN IMPORTED)
    set_target_properties(HepMC3::search PROPERTIES IMPORTED_LOCATION "${HEPMC3_SEARCH_LIB}" INTERFACE_INCLUDE_DIRECTORIES "${HEPMC3_INCLUDE_DIR}")
  endif()
  if (SHERPA_ENABLE_HEPMC3_ROOT)
    set(HepMC3_rootIO  HepMC3::rootIO)
  endif()
  message(STATUS "SHERPA: HEPMC3_VERSION=${HEPMC3_VERSION} HEPMC3_LIBRARIES=${HEPMC3_LIBRARIES}  HEPMC3_INCLUDE_DIR=${HEPMC3_INCLUDE_DIR}")
  add_subdirectory(AddOns/HepMC)
endif()
if (SHERPA_ENABLE_RIVET)
  add_subdirectory(AddOns/Rivet)
endif()
add_subdirectory(SHERPA)

set( IDIRS  AHADIC++ AMEGIC++ AMISIC++ ATOOLS BEAM COMIX CSSHOWER++ DIM DIRE EXTAMP EXTRA_XS HADRONS++
METOOLS MCATNLO MODEL PHASIC++ PHOTONS++  PDF RECONNECTIONS REMNANTS SHRiMPS SHERPA YFS)

if (SHERPA_ENABLE_MANUAL)
  add_subdirectory(Manual)
endif()

if (SHERPA_ENABLE_PYTHON)
  install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/AddOns/Python
              DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/SHERPA-MC${VERSIONING}/AddOns/ COMPONENT devel
              FILES_MATCHING PATTERN "*.h" PATTERN "*.H"
                             PATTERN "*\.c" EXCLUDE
                             )
endif()

if (SHERPA_ENABLE_ANALYSIS)
  install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/AddOns/Analysis
              DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/SHERPA-MC${VERSIONING}/AddOns/ COMPONENT devel
              FILES_MATCHING PATTERN "*.h" PATTERN "*.H"
                             PATTERN "*/Scripts" EXCLUDE
                             PATTERN "*\.c" EXCLUDE
                             )
endif()

if (SHERPA_ENABLE_EWSUD)
  install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/AddOns/EWSud
              DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/SHERPA-MC${VERSIONING}/AddOns/ COMPONENT devel
              FILES_MATCHING PATTERN "*.h" PATTERN "*.H"
                             PATTERN "*\.c" EXCLUDE
                             PATTERN "*/Tests" EXCLUDE
                             )
endif()
if (SHERPA_ENABLE_PYTHON OR SHERPA_ENABLE_DIHIGGS OR SHERPA_ENABLE_UFO OR (SHERPA_ENABLE_RIVET AND SHERPA_ENABLE_TESTING) )
  if (NOT  SHERPA-MC_PYTHON_VERSION)
    find_package (Python 3 COMPONENTS  Development Interpreter)
  else()
    find_package (Python ${SHERPA-MC_PYTHON_VERSION} EXACT COMPONENTS  Development Interpreter)
  endif()
  if (NOT SHERPA_Python_SITEARCH${Python_VERSION_MAJOR}${Python_VERSION_MINOR})
    set (SHERPA_Python_SITEARCH ${CMAKE_INSTALL_PREFIX}/lib/python${Python_VERSION_MAJOR}.${Python_VERSION_MINOR}/site-packages/)
    message(STATUS "SHERPA: Python_VERSION_MAJOR.Python_VERSION_MINOR=${Python_VERSION_MAJOR}${Python_VERSION_MINOR} SHERPA_Python_SITEARCH${Python_VERSION_MAJOR}${Python_VERSION_MINOR} defaults to ${SHERPA_Python_SITEARCH}")
  else()
    message(STATUS "SHERPA: Python_VERSION_MAJOR.Python_VERSION_MINOR=${Python_VERSION_MAJOR}${Python_VERSION_MINOR}Python_VERSION_MAJOR.Python_VERSION_MINOR=${Python_VERSION_MAJOR}${Python_VERSION_MINOR} SHERPA_Python_SITEARCH${Python_VERSION_MAJOR}${Python_VERSION_MINOR} is set to to ${SHERPA_Python_SITEARCH}")
  endif()
  set (PYTHON "${Python_EXECUTABLE}")
  if (SHERPA_ENABLE_PYTHON)
    set (PYLIBDIR "${SHERPA_Python_SITEARCH}")
    add_subdirectory(AddOns/Python)
  endif()
  if (SHERPA_ENABLE_DIHIGGS)
    add_subdirectory(AddOns/DiHiggsNLO)
  endif()
endif()
#This directory is unconditional install, but requires Python for extra files to be installed
add_subdirectory(MODEL/UFO)

foreach ( IDIR ${IDIRS} )
    install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${IDIR}
              DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/SHERPA-MC${VERSIONING}  COMPONENT devel
              FILES_MATCHING PATTERN "*.h" PATTERN "*.H" PATTERN "*.icc"
                             PATTERN "*\.c" EXCLUDE
                             PATTERN "*/fjcore.hh"
                             PATTERN "*Math/Sobol/*" EXCLUDE
                             PATTERN "*Math/Sobol" EXCLUDE
                             PATTERN "*EXTAMP/Example*" EXCLUDE
                             PATTERN "*MODEL/AQGC*" EXCLUDE
                             PATTERN "*MODEL/MUED*" EXCLUDE
                             PATTERN "*MODEL/SMDM*" EXCLUDE
                             PATTERN "*MODEL/SMGold*" EXCLUDE
                             PATTERN "*MODEL/TauPi*" EXCLUDE
                             PATTERN "*PDF/CT14*" EXCLUDE
                             PATTERN "*PDF/GRS/GRSGrid*" EXCLUDE
                             PATTERN "*PDF/SAL/SALGrid*" EXCLUDE
                             PATTERN "*PDF/CJK/CJKHOGrid*" EXCLUDE
                             PATTERN "*PDF/CJK/CJK1Grid*" EXCLUDE
                             PATTERN "*PDF/CJK/CJK2Grid*" EXCLUDE
                             PATTERN "*PDF/LHAPDF" EXCLUDE
                             PATTERN "*SHERPA/Run" EXCLUDE
                             PATTERN "*HADRONS++/Run" EXCLUDE
                             PATTERN "*makefile*" EXCLUDE
                             )
endforeach (  IDIR ${IDIRS}  )
# workaround for macOS
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/ATOOLS/Org/Node.C  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/SHERPA-MC${VERSIONING}/ATOOLS/Org/)
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/ATOOLS/Org/Getter_Function.C  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/SHERPA-MC${VERSIONING}/ATOOLS/Org/)
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/ATOOLS/Math/Cluster_Algorithm.C      DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/SHERPA-MC${VERSIONING}/ATOOLS/Math/)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/ATOOLS/Math/Sobol/21201 ${CMAKE_CURRENT_BINARY_DIR}/ATOOLS/Math/Sobol/21201)
if (SHERPA_ENABLE_GZIP)
  set(SOBOL_EXT .gz)
  add_custom_target( ZipSobol ALL)
#TAR IS INTERNAL CMAKE COMMAND!
  add_custom_command(TARGET ZipSobol PRE_BUILD
       COMMAND ${CMAKE_COMMAND} -E tar cfz 21201${SOBOL_EXT} --  21201
       WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/ATOOLS/Math/Sobol
       DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/ATOOLS/Math/Sobol/21201
       VERBATIM)
endif()
if (SHERPA_ENABLE_INTERNAL_PDFS)
  install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/PDF/CT14/CT14Grid DESTINATION  ${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING} COMPONENT doc PATTERN "*ct14ll*" EXCLUDE  )
  install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/PDF/SAL/SALGrid DESTINATION  ${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING} COMPONENT doc  )
  install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/PDF/CJK/CJKHOGrid DESTINATION  ${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING} COMPONENT doc  )
  install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/PDF/CJK/CJK1Grid DESTINATION  ${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING} COMPONENT doc  )
  install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/PDF/CJK/CJK2Grid DESTINATION  ${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING} COMPONENT doc  )
endif()
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/ATOOLS/Math/Sobol/21201${SOBOL_EXT} DESTINATION  ${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING}/Sobol COMPONENT doc  )
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/COMIX/Main/init_nlo.sh DESTINATION ${CMAKE_INSTALL_BINDIR} PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE )
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/SHERPA/Run/plot_graphs DESTINATION  ${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING} COMPONENT doc PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE )
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/PDF/NNPDF/NNPDF31_nnlo_as_0118_mc.tar.bz2 DESTINATION  ${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING} COMPONENT doc  )
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/COPYING ${CMAKE_CURRENT_SOURCE_DIR}/LICENCE DESTINATION  ${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING} COMPONENT doc PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ )

add_custom_target( unZipNNPDF31_nnlo_as_0118_mc ALL)
#TAR IS INTERNAL CMAKE COMMAND!
add_custom_command(TARGET unZipNNPDF31_nnlo_as_0118_mc PRE_BUILD
       COMMAND ${CMAKE_COMMAND} -E remove_directory ${CMAKE_CURRENT_BINARY_DIR}/NNPDF31_nnlo_as_0118_mc
       COMMAND ${CMAKE_COMMAND} -E tar xzf ${CMAKE_CURRENT_SOURCE_DIR}/PDF/NNPDF/NNPDF31_nnlo_as_0118_mc.tar.bz2
       WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
       DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/PDF/NNPDF/NNPDF31_nnlo_as_0118_mc.tar.bz2
       COMMENT "Unpacking NNPDF31_nnlo_as_0118_mc.tar.bz2"
       VERBATIM)
install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/NNPDF31_nnlo_as_0118_mc DESTINATION  ${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING}/ COMPONENT doc )

install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/AddOns/MadLoop/sconsloops DESTINATION  ${CMAKE_INSTALL_DATADIR}/SHERPA-MC${VERSIONING} COMPONENT doc PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE )
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/PHASIC++/Main/plot_stats.sh
                ${CMAKE_CURRENT_BINARY_DIR}/SHERPA/Run/Sherpa${VERSIONING}-config
                DESTINATION  ${CMAKE_INSTALL_BINDIR}  PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE )


add_library(SHERPAAll INTERFACE)
add_library(SHERPA::All ALIAS SHERPAAll)
set_target_properties(SHERPAAll PROPERTIES EXPORT_NAME SHERPA::All)
target_link_libraries(SHERPAAll INTERFACE
SherpaMain SherpaInitialization SherpaSingleEvents SherpaSoftPhysics SherpaPerturbativePhysics SherpaTools
ShrimpsMain ShrimpsEvents ShrimpsLadders ShrimpsBeamRemnants ShrimpsXsecs ShrimpsEikonals ShrimpsTools
HadronsMain HadronsMEs HadronsCurrents HadronsPSs
Reconnections
AhadicMain AhadicTools AhadicFormation AhadicDecays
Amisic AmisicTools AmisicPerturbative
PhotonsMain PhotonsTools PhotonsPhaseSpace PhotonsMEs PhotonsPhotonSplitter
Remnants  RemnantsTools
MCatNLOMain MCatNLOCalculators MCatNLOShowers MCatNLOTools
DIMTools DIMShower DIMGauge DIMLorentz DIMMain
CSMain CSCalculators CSShowers CSTools
DireTools DireShower DireGauge DireLorentz DireMain
PhasicScales Comix  ComixPhasespace ComixAmplitude
YFSCEEX  YFSMain YFSNLO  YFSTools
PhasicMain PhasicChannels PhasicProcess PhasicSelectors PhasicEnhance PhasicDecays
Amegic DipoleSubtraction Amplitude AmegicPSGen
String Zfunctions
ExtAmp ExtraXSNLO ExtraXSSpecial ExtraXS2_2 ExtraXS1_2 ExtraXS1_3 ExtraXS
METoolsExplicit METoolsCurrents METoolsVertices METoolsColors METoolsSpinCorrelations METoolsLoops METoolsMain ModelUFO ModelMain
PDF
BeamMain BeamSpectra
ToolsPhys ToolsMath ToolsYaml ToolsOrg
)
install(TARGETS SHERPAAll EXPORT SHERPATargets)

set(Sherpa_SRCS_C Sherpa.C Filter.C)
list(TRANSFORM Sherpa_SRCS_C PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/SHERPA/Main/")
add_executable(Sherpa  ${Sherpa_SRCS_C} ${CMAKE_CURRENT_SOURCE_DIR}/SHERPA/Run/Main.C)
target_link_libraries(Sherpa PRIVATE LibZip::LibZip SHERPA::All)
sherpa_handle_executable(Sherpa)
set_target_properties(Sherpa PROPERTIES OUTPUT_NAME Sherpa${VERSIONING})

if (SHERPA_ENABLE_HEPMC3)
  target_link_libraries(Sherpa PRIVATE SherpaHepMC3Output  HepMC3::HepMC3 HepMC3::search ${HepMC3_rootIO})
endif()

install(TARGETS Sherpa DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT libs)

add_subdirectory(AddOns/Weights)
add_subdirectory(AddOns/NNLO)
add_subdirectory(AddOns/Higgs)
if (SHERPA_ENABLE_EWSUD)
  add_subdirectory(AddOns/EWSud)
endif()
if (SHERPA_ENABLE_PYTHIA8)
  find_package(Pythia8 8.220 REQUIRED)
  message(STATUS "SHERPA: PYTHIA8_VERSION=${PYTHIA8_VERSION} PYTHIA8_LIBRARIES=${PYTHIA8_LIBRARIES} PYTHIA8_INCLUDE_DIRS=${PYTHIA8_INCLUDE_DIRS}")
  add_subdirectory(AddOns/Pythia)
endif()

if (SHERPA_ENABLE_ROOT)
  #This is a hack to prevent looking for the nlohmannjson
  set(ROOT_builtin_nlohmannjson_FOUND OK)
  find_package(ROOT 6.22 REQUIRED)
  set(CMAKE_CXX_FLAGS_BEFORE_ROOT "${CMAKE_CXX_FLAGS}")
  set(CMAKE_C_FLAGS_BEFORE_ROOT "${CMAKE_C_FLAGS}")
  set(CMAKE_Fortran_FLAGS_BEFORE_ROOT "${CMAKE_Fortran_FLAGS}")
  include(${ROOT_USE_FILE})
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS_BEFORE_ROOT}")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS_BEFORE_ROOT}")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_BEFORE_ROOT}")
  find_program(ROOT_CONFIG_EXE NAMES root-config PATHS ${ROOT_BINDIR}  NO_DEFAULT_PATH )

  set(ROOT_CONFIG_CPPFLAGS_STRING)
  set(ROOT_CONFIG_LIBS_STRING)
  if (ROOT_CONFIG_EXE)
    execute_process(COMMAND ${ROOT_CONFIG_EXE} --cflags OUTPUT_VARIABLE ROOT_CONFIG_CPPFLAGS_STRING OUTPUT_STRIP_TRAILING_WHITESPACE)
    execute_process(COMMAND ${ROOT_CONFIG_EXE} --libs   OUTPUT_VARIABLE ROOT_CONFIG_LIBS_STRING OUTPUT_STRIP_TRAILING_WHITESPACE)
  endif()
  message(STATUS "SHERPA: ROOT_VERSION=${ROOT_VERSION} ROOT_CONFIG_EXE=${ROOT_CONFIG_EXE} ROOT_LIBRARIES=${ROOT_LIBRARIES} ROOT_INCLUDE_DIRS=${ROOT_INCLUDE_DIRS} ROOT_CXX_FLAGS=${ROOT_CXX_FLAGS}")
  add_subdirectory(AddOns/Root)
endif()

if (SHERPA_ENABLE_BLACKHAT)
  find_package(BlackHat REQUIRED)
  message(STATUS "SHERPA: BLACKHAT_VERSION=${BLACKHAT_VERSION} BLACKHAT_PATH=${BLACKHAT_PATH}  BLACKHAT_LIBRARIES=${BLACKHAT_LIBRARIES} BLACKHAT_INCLUDE_DIRS=${BLACKHAT_INCLUDE_DIRS}")
  add_subdirectory(AddOns/BlackHat)
endif()

if (SHERPA_ENABLE_MADLOOP)
  if (MADLOOP_DIR)
    find_path(MADLOOP_PREFIX  mg5_aMC PATHS ${MADLOOP_DIR} PATH_SUFFIXES bin  REQUIRED NO_DEFAULT_PATH)
  else()
    find_program( MG5AMC NAMES  mg5_aMC REQUIRED)
    if (MG5AMC)
      get_filename_component (MADLOOP_PREFIX_BIN ${MG5AMC} DIRECTORY)
      get_filename_component (MADLOOP_PREFIX ${MADLOOP_PREFIX_BIN} DIRECTORY)
    endif()
  endif()
  message(STATUS "SHERPA: MADLOOP_PREFIX=${MADLOOP_PREFIX}")
  add_subdirectory(AddOns/MadLoop)
endif()

if (SHERPA_ENABLE_LHOLE)
  add_subdirectory(AddOns/LH_OLE)
endif()

if (SHERPA_ENABLE_OPENLOOPS)
#Version is not set to allow for "beta"
  find_package(OpenLoops REQUIRED)
  message(STATUS "SHERPA: OPENLOOPS_PREFIX=${OPENLOOPS_PREFIX} OPENLOOPS_LIBRARY=${OPENLOOPS_LIBRARY}")
  add_subdirectory(AddOns/OpenLoops)
endif()

if (SHERPA_ENABLE_RECOLA)
  find_package(Recola REQUIRED)
  message(STATUS "SHERPA: RECOLA_VERSION=${RECOLA_VERSION} RECOLA_PREFIX=${RECOLA_PREFIX} RECOLA_INCLUDE_DIR=${RECOLA_INCLUDE_DIR}")
  add_subdirectory(AddOns/Recola)
endif()

if (SHERPA_ENABLE_GOSAM)
  find_package(GoSam REQUIRED)
  message(STATUS "SHERPA: GOSAM_VERSION=${GOSAM_VERSION} GOSAM_PATH=${GOSAM_PREFIX}")
  add_subdirectory(AddOns/GoSam)
endif()

IF (SHERPA_ENABLE_ANALYSIS)
  add_subdirectory(AddOns/Analysis)
endif()

if (SHERPA_ENABLE_DOXYGEN)
  find_package(Doxygen 1.9.3 REQUIRED)
  message(STATUS "SHERPA: DOXYGEN_VERSION=${DOXYGEN_VERSION}")
  set(DOXYGEN_OUTPUT_DIRECTORY docs)
  set(DOXYGEN_FILE_PATTERNS *.C *.H *.c *.h *.cc *.hh *.cpp *.hpp)
  set(DOXYGEN_EXCLUDE_PATTERNS */.*)
  set(DOXYGEN_EXCLUDE
    # weird stuff that's in the build but duplicated
    MODEL/HEFT/SVV_LC.C
    MODEL/HEFT/SVVP_LC.C
    MODEL/HEFT/SVVV_LC.C
    MODEL/HEFT/SVVVV_LC.C
    # headers that can only be included after other stuff
    ATOOLS/Math/Tensor_Build.H
    ATOOLS/Math/Tensor_Contractions.H
    ATOOLS/Math/Tensor_Contractions_Epsilon.H
  )
  set(DOXYGEN_MACRO_EXPANSION YES)
  set(DOXYGEN_EXPAND_ONLY_PREDEF YES)
  set(DOXYGEN_EXPAND_AS_DEFINED
    DECLARE_GETTER_BASE
    DECLARE_ND_GETTER
    DECLARE_GETTER
    DECLARE_XSBASE_GETTER
    DECLARE_CPL_GETTER
    DECLARE_VIRTUALME2_GETTER
    DECLARE_TREEME2_GETTER
    DECLARE_PDF_GETTER
    DEFINE_CURRENT_GETTER
    DEFINE_ME_GETTER
    DEFINE_QUALIFIER_GETTER
    DEFINE_UNARY_BINARY_OPERATOR
    DEFINE_FUNCTION
    DEFINE_GETTER_METHOD
    DEFINE_QUALIFIER_CLASS
    PROMOTE
  )
  # YES if you want private members in the documentation
  set(DOXYGEN_EXTRACT_PRIVATE YES)
  # NO if you want undocumented members to have empty documentation
  set(DOXYGEN_HIDE_UNDOC_MEMBERS NO)
  set(DOXYGEN_EXAMPLE_PATH Examples)
  set(DOXYGEN_EXAMPLE_PATTERNS *.cpp)
  set(DOXYGEN_EXAMPLE_RECURSIVE YES)
  set(DOXYGEN_CITE_BIB_FILES Manual/source/manual/references.bib)
  set(DOXYGEN_CLASS_GRAPH YES)
  set(DOXYGEN_HAVE_DOT NO)
  set(DOXYGEN_WARN_IF_INCOMPLETE_DOC NO)
  set(DOXYGEN_WARN_IF_UNDOCUMENTED NO)
  set(DOXYGEN_WARN_NO_PARAMDOC NO)
  set(DOXYGEN_QUIET YES)
  set(DOXYGEN_USE_MATHJAX YES)
  set(DOXYGEN_INCLUDE_PATH
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}
    ${LHAPDF_INCLUDE_DIRS}
  )
  set(DOXYGEN_ENABLED_SECTIONS
    # enable @cond-ed out sections by uncommenting these lines:
    #DOCUMENT_COMMENTED_OUT_FUNCTION
    #RECURSIVE_WARNING
    #ANTI_INCLUDE_GUARD
    #TEMPLATE_INSTANTIATIONS
  )
  set(DOXYGEN_CLANG_ASSISTED_PARSING NO)
  set(DOXYGEN_CLANG_ADD_INC_PATHS NO)
  get_property(ALL_SOURCE_FILES GLOBAL PROPERTY ALL_SOURCE_FILES)
  doxygen_add_docs(docs ${ALL_SOURCE_FILES})
endif()

########################################################################
# Config files
get_filename_component (CXX "${CMAKE_CXX_COMPILER}" NAME)
set(CXXFLAGS "${CMAKE_CXX_FLAGS} ${CMAKE_CXX_FLAGS_${CMAKE_BUILD_TYPE}}")
message(STATUS "Using for the script CXX=${CXX} CXXFLAGS=${CXXFLAGS}")
get_filename_component (CC "${CMAKE_C_COMPILER}" NAME)
set (CFLAGS ${CMAKE_C_FLAGS})
message(STATUS "Using for the script CC=${CC} CFLAGS=${CFLAGS}")
set (FCFLAGS ${CMAKE_Fortran_FLAGS})
if (SHERPA_ENABLE_FORTRAN)
  get_filename_component (FC "${CMAKE_Fortran_COMPILER}" NAME)
endif()

set( LD_LIBRARY_PATH "LD_LIBRARY_PATH")
if (CMAKE_SYSTEM_NAME MATCHES Darwin)
  set( LD_LIBRARY_PATH "DYLD_LIBRARY_PATH")
endif()

if (SHERPA_ENABLE_HEPMC3)
  set(CONDITIONAL_HEPMC3INCS "-I${HEPMC3_INCLUDE_DIR}")
  get_filename_component (HEPMC3_LIBRARY_DIR ${HEPMC3_LIB} DIRECTORY)
  if (SHERPA_ENABLE_HEPMC3_ROOT)
    get_filename_component (HEPMC3_ROOTIO_LIBRARY_DIR ${HEPMC3_ROOTIO_LIB} DIRECTORY)
    set(CONDITIONAL_HEPMC3LIBS "-L${HEPMC3_LIBRARY_DIR} -lHepMC3 -L${HEPMC3_ROOTIO_LIBRARY_DIR} -lHepMC3rootIO")
  else()
    set(CONDITIONAL_HEPMC3LIBS "-L${HEPMC3_LIBRARY_DIR} -lHepMC3")
  endif()
endif()

########################################################################
set(CXXFLAGS_PACKAGES_H_CONTENT
"/* ATOOLS/Org/CXXFLAGS_PACKAGES.H.  Generated from CXXFLAGS_PACKAGES.H.in by configure.  */\n\
#ifndef CXXFLAGS_PACKAGES_H\n\
#define CXXFLAGS_PACKAGES_H\n\n\
/* Using HEPMC3 */\n\
$<IF:$<BOOL:${HepMC3_FOUND}>,#define USING__HEPMC3 \"1\",/* #undef USING__HEPMC3 */>\n\n\
/* HepMC3 with ROOT support */\n\
$<IF:$<BOOL:${SHERPA_ENABLE_HEPMC3_ROOT}>,#define USING__HEPMC3__ROOT \"1\",/* #undef USING__HEPMC3__ROOT */>\n\n\
/* using gzip */\n\
$<IF:$<BOOL:${SHERPA_ENABLE_GZIP}>,#define USING__GZIP \"1\",/* #undef USING__GZIP */>\n\n\
/* using LHAPDF */\n\
$<IF:$<BOOL:${SHERPA_ENABLE_LHAPDF}>,#define USING__LHAPDF \"1\",/* #undef USING__LHAPDF */>\n\n\
/* LHAPDF PATH */\n\
$<IF:$<BOOL:${SHERPA_ENABLE_LHAPDF}>,#define LHAPDF_PATH \"${LHAPDF_PATH}\",/* #undef LHAPDF_PATH */>\n\n\
/* using ROOT */\n\
$<IF:$<BOOL:${SHERPA_ENABLE_ROOT}>,#define USING__ROOT \"1\",/* #undef USING__ROOT */>\n\n\
/* using Rivet */\n\
$<IF:$<BOOL:${SHERPA_ENABLE_RIVET}>,#define USING__RIVET \"1\",/* #undef USING__RIVET */>\n\n\
/* using RIVET4 */\n\
$<IF:$<BOOL:$<VERSION_GREATER_EQUAL:${RIVET_VERSION},4.0.0>>,#define USING__RIVET4 \"1\",/* #undef USING__RIVET4 */>\n\n\
/* using BLACKHAT */\n\
$<IF:$<BOOL:${SHERPA_ENABLE_BLACKHAT}>,#define USING__BLACKHAT \"1\",/* #undef USING__BLACKHAT */>\n\n\
/* BLACKHAT directory */\n\
$<IF:$<BOOL:${SHERPA_ENABLE_BLACKHAT}>,#define BLACKHAT_PATH \"${BLACKHAT_PATH}\",/* #undef BLACKHAT_PATH */>\n\n\
/* OpenLoops installation prefix */\n\
$<IF:$<BOOL:${SHERPA_ENABLE_OPENLOOPS}>,#define OPENLOOPS_PREFIX \"${OPENLOOPS_PREFIX}\",/* #undef OPENLOOPS_PREFIX */>\n\n\
/* GoSam installation prefix */\n\
$<IF:$<BOOL:${SHERPA_ENABLE_GOSAM}>,#define GOSAM_PREFIX \"${GOSAM_PREFIX}\",/* #undef GOSAM_PREFIX */>\n\n\
/* MadLoop installation prefix */\n\
$<IF:$<BOOL:${SHERPA_ENABLE_MADLOOP}>,#define MADLOOP_PREFIX \"${MADLOOP_PREFIX}\",/* #undef MADLOOP_PREFIX */>\n\n\
/* Recola installation prefix */\n\
$<IF:$<BOOL:${SHERPA_ENABLE_RECOLA}>,#define RECOLA_PREFIX \"${RECOL_PREFIX}\",/* #undef RECOLA_PREFIX */>\n\n\
/* using MCFM */\n\
$<IF:$<BOOL:${SHERPA_ENABLE_MCFM}>,#define USING__MCFM \"1\",/* #undef USING__MCFM */>\n\n\
/* MCFM directory */\n\
$<IF:$<BOOL:${SHERPA_ENABLE_MCFM}>,#define MCFM_PATH \"${MCFM_PATH}\",/* #undef MCFM_PATH */>\n\n\
/* using EWSud */\n\
$<IF:$<BOOL:${SHERPA_ENABLE_EWSUD}>,#define USING__EWSud \"1\",/* #undef USING__EWSud */>\n\n\
#endif")
file(GENERATE OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/ATOOLS/Org/CXXFLAGS_PACKAGES.H CONTENT "${CXXFLAGS_PACKAGES_H_CONTENT}")
########################################################################
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/AMEGIC++/Main/makelibs.in ${CMAKE_CURRENT_BINARY_DIR}/AMEGIC++/Main/makelibs @ONLY)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/AMEGIC++/Main/makelibs    DESTINATION  ${CMAKE_INSTALL_DATADIR}/SHERPA-MC COMPONENT doc PERMISSIONS  OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE )
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/SHERPA/Run/Sherpa-config.in ${CMAKE_CURRENT_BINARY_DIR}/SHERPA/Run/Sherpa${VERSIONING}-config @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/EXTAMP/Example/madgraph/CMakeLists.txt.in ${CMAKE_CURRENT_BINARY_DIR}/EXTAMP/Example/madgraph/CMakeLists.txt @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/EXTAMP/Example/subtraction/CMakeLists.txt.in ${CMAKE_CURRENT_BINARY_DIR}/EXTAMP/Example/subtraction/CMakeLists.txt @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Examples/V_plus_Bs/LHC_Wbb/CMakeLists.txt.in ${CMAKE_CURRENT_BINARY_DIR}/Examples/V_plus_Bs/LHC_Wbb/CMakeLists.txt @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Examples/V_plus_Jets/LHC_WJets/CMakeLists.txt.in ${CMAKE_CURRENT_BINARY_DIR}/Examples/V_plus_Jets/LHC_WJets/CMakeLists.txt @ONLY)

if (SHERPA_ENABLE_UFO)
SET(SHERPA_CMAKE_CONFIG_DIR ${CMAKE_INSTALL_PREFIX}/share/SHERPA-MC/)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/MODEL/UFO/src/ufo_interface/parser.py.in ${CMAKE_CURRENT_BINARY_DIR}/MODEL/UFO/src/ufo_interface/parser.py @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/MODEL/UFO/bin/Sherpa-generate-model.in ${CMAKE_CURRENT_BINARY_DIR}/MODEL/UFO/bin/Sherpa-generate-model @ONLY)
if (NOT SHERPA_ENABLE_PYTHON AND SHERPA_ENABLE_TESTING)
    message(FATAL_ERROR "SHERPA: UFO model generation tests requires Python Addons!")
endif()
if(SHERPA_ENABLE_TESTING)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/MODEL/UFO/tests/test_lorentz.py.in ${CMAKE_CURRENT_BINARY_DIR}/Tests/UFO/test_lorentz.py @ONLY)

file(DOWNLOAD https://zenodo.org/records/12533470/files/TestProcesses.tar.gz?download=1 ${CMAKE_CURRENT_BINARY_DIR}/Tests/End2End/UFO/TestProcesses.tar.gz)
add_custom_target( unZipTestProcesses ALL)
add_custom_command(TARGET unZipTestProcesses PRE_BUILD
     COMMAND ${CMAKE_COMMAND} -E remove_directory ${CMAKE_CURRENT_BINARY_DIR}/Tests/End2End/UFO/TestProcesses
     COMMAND ${CMAKE_COMMAND} -E tar xzf ${CMAKE_CURRENT_BINARY_DIR}/Tests/End2End/UFO/TestProcesses.tar.gz
     WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Tests/End2End/UFO
     DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/Tests/End2End/UFO/TestProcesses.tar.gz
     COMMENT "Unpacking UFO Test Processes"
     VERBATIM)
endif()
endif()
install(EXPORT SHERPATargets DESTINATION ${CMAKE_INSTALL_DATADIR}/SHERPA-MC)

########################################################################
include(CMakePackageConfigHelpers)
configure_package_config_file(cmake/Templates/SHERPA-MCConfig.cmake.in ${PROJECT_BINARY_DIR}/outputs/share/SHERPA-MC/SHERPA-MCConfig.cmake
                              INSTALL_DESTINATION ${CMAKE_INSTALL_DATADIR}/SHERPA-MC)
write_basic_package_version_file(${PROJECT_BINARY_DIR}/outputs/share/SHERPA-MC/SHERPA-MCConfig-version.cmake
                                 COMPATIBILITY SameMajorVersion VERSION ${PROJECT_VERSION})
install(FILES ${PROJECT_BINARY_DIR}/outputs/share/SHERPA-MC/SHERPA-MCConfig.cmake
              ${PROJECT_BINARY_DIR}/outputs/share/SHERPA-MC/SHERPA-MCConfig-version.cmake
              DESTINATION ${CMAKE_INSTALL_DATADIR}/SHERPA-MC COMPONENT devel)
export(EXPORT SHERPATargets  FILE ${PROJECT_BINARY_DIR}/outputs/share/SHERPA-MC/SHERPATargets.cmake)
########################################################################
if (CMAKE_SYSTEM_NAME MATCHES Darwin)
  set(DARWIN_SYS 1)
endif()
if (CMAKE_SYSTEM_NAME MATCHES "Linux")
  set(LINUX_SYS 1)
endif()
if (UNIX AND ( NOT  CMAKE_SYSTEM_NAME MATCHES "Linux")  AND (NOT (CMAKE_SYSTEM_NAME MATCHES Darwin)))
  set(UNIX_SYS 1)
endif()
set(CXXFLAGS_H_CONTENT "#ifndef ATOOLS_Org_CXXFLAGS_H\n#define ATOOLS_Org_CXXFLAGS_H\n\n\
/* ATOOLS/Org/CXXFLAGS.H. Automatically generated from CMakeLists.txt. */\n\n\
/* Architecture identified as Darwin MacOS */\n\
$<IF:$<BOOL:${DARWIN_SYS}>,#define ARCH_DARWIN \"1\",/* #undef ARCH_DARWIN */>\n\n\
/* Architecture identified as Linux */\n\
$<IF:$<BOOL:${LINUX_SYS}>,#define ARCH_LINUX 1,/* #undef ARCH_LINUX */>\n\n\
/* Architecture identified as Unix */\n\
$<IF:$<BOOL:${UNIX_SYS}>,#define ARCH_UNIX 1,/* #undef ARCH_UNIX */>\n\n\
/* Define to dummy `main' function (if any) required to link to the Fortran\n   libraries. */\n\
/* #undef FC_DUMMY_MAIN */\n\n\
/* Define if F77 and FC dummy `main' functions are identical. */\n\
/* #undef FC_DUMMY_MAIN_EQ_F77 */\n\n\
/* MPI support */\n\
$<IF:$<BOOL:${SHERPA_ENABLE_MPI}>,#define USING__MPI \"1\",/* #undef USING__MPI */>\n\n\
/* Thread support */\n\
$<IF:$<BOOL:${SHERPA_ENABLE_THREADING}>,#define USING__Threading \"1\",/* #undef USING__Threading */>\n\n\
/* ld path name */\n\
#define LD_PATH_NAME \"${LD_LIBRARY_PATH}\"\n\n\
/* library suffix */\n\
#define LIB_SUFFIX \"${CMAKE_SHARED_LIBRARY_SUFFIX}\"\n\n\
/* binreloc activation */\n\
$<IF:$<BOOL:${SHERPA_ENABLE_BINRELOC}>,#define ENABLE_BINRELOC \"1\",/* #undef ENABLE_BINRELOC */>\n\n\
/* Sherpa installation prefix */\n\
#define SHERPA_PREFIX \"${CMAKE_INSTALL_PREFIX}\"\n\n\
/* Sherpa include directory */\n\
#define SHERPA_INCLUDE_PATH \"${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_INCLUDEDIR}/SHERPA-MC\"\n\n\
/* Sherpa include subdirectory */\n\
#define SHERPA_INCLUDE_SUBDIR \"${CMAKE_INSTALL_INCLUDEDIR}/SHERPA-MC\"\n\n\
/* Sherpa library directory */\n\
#define SHERPA_LIBRARY_PATH \"${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/SHERPA-MC/\"\n\n\
/* Sherpa library subdirectory */\n\
#define SHERPA_LIBRARY_SUBDIR \"${CMAKE_INSTALL_LIBDIR}/SHERPA-MC/\"\n\n\
/* Sherpa data directory */\n\
#define SHERPA_SHARE_PATH \"${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/SHERPA-MC/\"\n\n\
/* Sherpa subversion */\n\
#define SHERPA_SHARE_SUBDIR \"${CMAKE_INSTALL_DATADIR}/SHERPA-MC/\"\n\n\
/* Sherpa subversion */\n\
#define SHERPA_SUBVERSION \"${SHERPA_VERSION_MINOR}.${SHERPA_VERSION_PATCH}\"\n\n\
/* Sherpa version */\n\
#define SHERPA_VERSION \"${SHERPA_VERSION_MAJOR}\"\n\n\
/* Sherpa version name */\n\
#define SHERPA_NAME \"Erebus\"\n\n\
/* Sherpa version code */\n\
#define SHERPA_VERSION_CODE ${SHERPA_VERSION_CODE}\n\n\
/* Using colour */\n\
#define USING__COLOUR \"1\"\n\n\
#define PYTHON_LIBS \"${SHERPA_Python_SITEARCH}\"\n\n\
#endif\n")
file(GENERATE OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/ATOOLS/Org/CXXFLAGS.H CONTENT "${CXXFLAGS_H_CONTENT}")
########################################################################
install(FILES
      ${CMAKE_CURRENT_BINARY_DIR}/ATOOLS/Org/CXXFLAGS.H
      ${CMAKE_CURRENT_BINARY_DIR}/ATOOLS/Org/CXXFLAGS_PACKAGES.H DESTINATION
      ${CMAKE_INSTALL_INCLUDEDIR}/SHERPA-MC/ATOOLS/Org/  COMPONENT devel )
########################################################################
if (SHERPA_ENABLE_TESTING)
  message(STATUS "SHERPA: Will use the installed SHERPA for testing! Run ctest only after the installation!")
  set(STANDARDENVIRONMENT "SHERPA_CMAKE_PATH=${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/SHERPA-MC;LHAPDF_DATA_PATH=${LHAPDF_DATA_PATH}:$ENV{LHAPDF_DATA_PATH};SHERPA_SHARE_PATH=${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/SHERPA-MC")
  list(APPEND STANDARDENVIRONMENT "PATH=${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/SHERPA-MC/:$ENV{PATH}")
  if (SHERPA_ENABLE_RIVET)
    list(JOIN RIVET_CONFIG_LIB_DIRS ":" RIVET_RUNTIME_LIBS)
    #string(REGEX_REPLACE ":$" "" RIVET_RUNTIME_LIBS  "${RIVET_RUNTIME_LIBS}")
    list(APPEND STANDARDENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/SHERPA-MC/:${LHAPDF_LIBRARY_DIR}:$ENV{LD_LIBRARY_PATH}")
    list(APPEND STANDARDENVIRONMENT "DYLD_LIBRARY_PATH=${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/SHERPA-MC/:${LHAPDF_LIBRARY_DIR}:$ENV{DYLD_LIBRARY_PATH}")
  else()
    list(APPEND STANDARDENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/SHERPA-MC/:${LHAPDF_LIBRARY_DIR}:${RIVET_RUNTIME_LIBS}:$ENV{LD_LIBRARY_PATH}")
    list(APPEND STANDARDENVIRONMENT "DYLD_LIBRARY_PATH=${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/SHERPA-MC/:${LHAPDF_LIBRARY_DIR}:${RIVET_RUNTIME_LIBS}:$ENV{DYLD_LIBRARY_PATH}")
  endif()
  set (makelibsscript ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/SHERPA-MC/makelibs)
  message(STATUS "SHERPA: STANDARDENVIRONMENT=${STANDARDENVIRONMENT}")
  ENABLE_TESTING()
  add_subdirectory(Tests)
  if (SHERPA_ENABLE_EWSUD)
    add_subdirectory(AddOns/EWSud/Tests)
  endif()
endif()

if (SHERPA_ENABLE_EXAMPLES)
  add_subdirectory(Examples)
endif()

include(ConfigLogger)
