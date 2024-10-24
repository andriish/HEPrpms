cmake_minimum_required(VERSION 3.10)
project(qqvvamp LANGUAGES CXX)
#Andrii Verbytskyi for VVamp project

# Set the version number
set(VERSION_MAJOR 1)
set(VERSION_MINOR 1)
set(VERSION_PATCH 1)

# Define the library version
set(LIB_VERSION ${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH})

# Add cmake/Modules to the module path
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules ${CMAKE_MODULE_PATH})

# Set the C++ standard
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Define the source files
file(GLOB SOURCES
"qqvvamp.cpp"
"qqvvampprec.cpp"
"p*.cpp"
)

# Find the GiNaC libraries
find_package(GiNaC REQUIRED)
# Determine if we are on an ARM architecture
if(CMAKE_SYSTEM_PROCESSOR MATCHES "arm" OR CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64")
set(LIBQUADMATH )
else()
set(LIBQUADMATH quadmath)
endif()


# Include GNUInstallDirs for standardized installation directories
include(GNUInstallDirs)

# Options to build static, shared, or both libraries
option(BUILD_SHARED_LIBS "Build shared libraries" ON)
option(BUILD_STATIC_LIBS "Build static libraries" OFF)

# Create the library targets based on options
if(BUILD_SHARED_LIBS)
add_library(qqvvamp SHARED ${SOURCES})
set_target_properties(qqvvamp PROPERTIES VERSION ${LIB_VERSION} SOVERSION 1)
target_link_libraries(qqvvamp ginac::ginac ${LIBQUADMATH})
target_include_directories(qqvvamp
PUBLIC
$<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}>
$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)
install(TARGETS qqvvamp
EXPORT qqvvampTargets
ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
)
endif()

if(BUILD_STATIC_LIBS)
add_library(qqvvamp_static STATIC ${SOURCES})
set_target_properties(qqvvamp_static PROPERTIES OUTPUT_NAME "qqvvamp")
target_link_libraries(qqvvamp_static ginac::ginac ${LIBQUADMATH})
target_include_directories(qqvvamp_static
PUBLIC
$<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}>
$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)
install(TARGETS qqvvamp_static
EXPORT qqvvampTargets
ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
)
endif()

# Define the executable sources
set(EXEC_SOURCES evalcmdline.cpp)

# Create the executables
if(BUILD_SHARED_LIBS)
add_executable(evalcmdline ${EXEC_SOURCES})
target_link_libraries(evalcmdline qqvvamp ginac::ginac ${LIBQUADMATH})
endif()

if(BUILD_STATIC_LIBS)
add_executable(evalcmdline_static ${EXEC_SOURCES})
target_link_libraries(evalcmdline_static qqvvamp_static ginac::ginac ${LIBQUADMATH})
set_target_properties(evalcmdline_static PROPERTIES OUTPUT_NAME "evalcmdline-static")
endif()

# Installation of headers
install(FILES ${CMAKE_SOURCE_DIR}/qqvvamp.h DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

# Create and install the CMake config file
include(CMakePackageConfigHelpers)
write_basic_package_version_file(
"${CMAKE_CURRENT_BINARY_DIR}/qqvvampConfigVersion.cmake"
VERSION ${LIB_VERSION}
COMPATIBILITY AnyNewerVersion
)

file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/qqvvampConfig.cmake" "
@PACKAGE_INIT@
include(CMakeFindDependencyMacro)
include(\"\${CMAKE_CURRENT_LIST_DIR}/qqvvampTargets.cmake\")
")

# Export the targets to a file
install(EXPORT qqvvampTargets
FILE qqvvampTargets.cmake
NAMESPACE qqvvamp::
DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/cmake/qqvvamp
)
install(FILES
"${CMAKE_CURRENT_BINARY_DIR}/qqvvampConfig.cmake"
"${CMAKE_CURRENT_BINARY_DIR}/qqvvampConfigVersion.cmake"
DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/cmake/qqvvamp
)