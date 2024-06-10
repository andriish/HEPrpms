#-------------------------------------------------------------------------------
# Author(s):
#    Simone Alioli, Frank Tackmann
#
# Copyright:
#    Copyright (C) 2017, 2018 Geneva Collaboration
#
#    This file is part of the Geneva MC framework. Geneva is distributed under
#    the terms of the GNU General Public License version 3 (GPLv3), see the
#    COPYING file that comes with this distribution for details.
#    Please respect the academic usage guidelines in the GUIDELINES file.
#
# Description:
#    Main cmake file for Geneva python library and driver
#-------------------------------------------------------------------------------

# define option whether to use python driver (turned on by default)
option (${PROJECT_NAME}_enable_python "Enable Geneva Python interface" ON)

# search for python 2 interpreter
set (${PROJECT_NAME}_USE_PYTHON FALSE)
if (${PROJECT_NAME}_enable_python)
   message ("== Configuring Geneva Python interface")

   find_package (PythonInterp EXACT 2.7)
   if (PYTHONINTERP_FOUND)
      set (${PROJECT_NAME}_USE_PYTHON TRUE)
   else ()
      message ("ww Could not find a valid Python interpreter, continuing without.")
      message (">> Please install Python 2.7 to enable the Geneva Python interface.")
   endif ()
endif ()

if (NOT ${PROJECT_NAME}_USE_PYTHON)
   return ()
endif ()

#-------------------------------------------------------------------------------
# setup
#-------------------------------------------------------------------------------

set (python_BUILD_DIR "${CMAKE_CURRENT_BINARY_DIR}/build")

# place python scripts and lib in subdirectories
set (python_dir "python${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR}/site-packages")
set (python_LIB_DIR "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${python_dir}")
set (python_BIN_DIR "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}")

# convert to Python-compliant normalized version string
string (REPLACE "-RC" "rc" geneva_PYTHON_VERSION "${geneva_VERSION}")
string (REGEX REPLACE "^([^-]*)-+(.*)" "\\1+\\2" geneva_PYTHON_VERSION "${geneva_PYTHON_VERSION}")
string (REPLACE "-" "." geneva_PYTHON_VERSION "${geneva_PYTHON_VERSION}")

# configure/generate necessary files
set (python_ENV_SCRIPT "geneva-env.sh")
file (WRITE "${CMAKE_CURRENT_BINARY_DIR}/${python_ENV_SCRIPT}"
   "export PYTHONPATH=\"${python_LIB_DIR}:$PYTHONPATH\"\n"
)

file (WRITE "${python_BUILD_DIR}-install/${python_ENV_SCRIPT}"
   "export PYTHONPATH=\"${CMAKE_INSTALL_PREFIX}/lib/${python_dir}:$PYTHONPATH\"\n"
)

#-------------------------------------------------------------------------------
# make the scripts
#-------------------------------------------------------------------------------

# list of inputs and outputs for scripts
set (python_bins
   bin/geneva
   bin/geneva-combine-plots
   bin/geneva-lhef-reweight
   bin/geneva-lhef-xsec
   bin/geneva-main
   bin/geneva-NERSC
   bin/geneva-setup-combine-grids
   bin/geneva-setup-combine-xsec
   bin/geneva-setup-xsec
)

if (${PROJECT_NAME}_USE_PYTHIA8)
   set (python_bins ${python_bins}
      bin/geneva-lhef-analyze
      bin/geneva-shower-pythia8
   )
endif()

if (${PROJECT_NAME}_USE_RIVET)
   set (python_bins ${python_bins} bin/geneva-rivet-analyze)
endif()

# add the env script
set (geneva_PYTHON_SCRIPTS "'${python_ENV_SCRIPT}'")
set (python_bins_output "${python_BIN_DIR}/${python_ENV_SCRIPT}")
set (python_bins_install "${python_BUILD_DIR}-install/${python_ENV_SCRIPT}")

foreach (file ${python_bins})
   set (geneva_PYTHON_SCRIPTS "${geneva_PYTHON_SCRIPTS},\n '${CMAKE_CURRENT_SOURCE_DIR}/${file}'")
   get_filename_component (file ${file} NAME)
   list (APPEND python_bins_output "${python_BIN_DIR}/${file}")
   list (APPEND python_bins_install "${python_BIN_DIR}/${file}")
endforeach ()

if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
   # workaround for newer Macs stripping DYLD_LIBRARY_PATH from the environment
   if (${CMAKE_SYSTEM_VERSION} GREATER 14)
      set (REPLACE_DYLD_LIBRARY_PATH "env[\"DYLD_LIBRARY_PATH\"] = env[\"GENEVA_EXTERNAL_LIBRARY_PATH\"]")
   endif ()
endif()

# custom command and target to build scripts
add_custom_command (
   OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/python-scripts" ${python_bins_output}
   COMMAND ${PYTHON_EXECUTABLE} setup.py build_scripts -f --build-dir ${python_BIN_DIR}
   COMMAND ${CMAKE_COMMAND} -E touch "${CMAKE_CURRENT_BINARY_DIR}/python-scripts"
   DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${python_ENV_SCRIPT}" ${python_bins}
   COMMENT "Setting up Python driver scripts"
)

add_custom_target (${PROJECT_NAME}-python-scripts DEPENDS ${python_bins_output})

install (PROGRAMS ${python_bins_install} DESTINATION bin)

if (TARGET ${PROJECT_NAME}-bin)
   add_dependencies (${PROJECT_NAME}-bin ${PROJECT_NAME}-python-scripts)
endif ()

#-------------------------------------------------------------------------------
# make the library
#-------------------------------------------------------------------------------

get_target_property (python_includes Wrapper INTERFACE_INCLUDE_DIRECTORIES)

foreach (dir ${python_includes})
   set (geneva_PYTHON_INCLUDE_DIRS "${geneva_PYTHON_INCLUDE_DIRS}'${dir}',\n")
endforeach ()
string(REGEX REPLACE ",\n$" "" geneva_PYTHON_INCLUDE_DIRS "${geneva_PYTHON_INCLUDE_DIRS}")

get_target_property (python_compile_args Wrapper COMPILE_OPTIONS)
set (python_compile_args ${python_compile_args} "-std=c++11")

if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
   # workaround for newer Macs using libc++ as default standard C++ library
   if (CMAKE_OSX_DEPLOYMENT_TARGET)
      set (python_compile_args ${python_compile_args} "-mmacosx-version-min=${CMAKE_OSX_DEPLOYMENT_TARGET}")
   endif()
endif()

foreach (flag ${python_compile_args})
   set (geneva_PYTHON_EXTRA_COMPILE_ARGS "${geneva_PYTHON_EXTRA_COMPILE_ARGS}'${flag}',\n")
endforeach ()
string(REGEX REPLACE ",\n$" "" geneva_PYTHON_EXTRA_COMPILE_ARGS "${geneva_PYTHON_EXTRA_COMPILE_ARGS}")

# this uses geneva_PYTHON_INCLUDE_DIRS and geneva_PYTHON_SCRIPTS
configure_file (
   "${CMAKE_CURRENT_SOURCE_DIR}/setup.py.in"
   "${CMAKE_CURRENT_BINARY_DIR}/setup.py"
)

configure_file (
   "${CMAKE_CURRENT_SOURCE_DIR}/config.py.in"
   "${python_BUILD_DIR}/genevapylib/config.py"
)

file (GLOB python_sources "genevapylib/*.py")
list (APPEND python_sources "${python_BUILD_DIR}/genevapylib/config.py")

foreach (file ${python_sources})
   get_filename_component (file ${file} NAME)
   list (APPEND python_lib_output "${python_LIB_DIR}/genevapylib/${file}")
   list (APPEND python_lib_output "${python_LIB_DIR}/genevapylib/${file}c")
endforeach ()

# add the cpp_extensions shared library generated by setup.py
# (this should eventually be handled by cmake itself)
list (APPEND python_sources "extensions.cpp")

set (python_lib_name "genevapylib/cpp_extensions${CMAKE_SHARED_LIBRARY_SUFFIX}")
list (APPEND python_lib_output "${python_LIB_DIR}/${python_lib_name}")
list (APPEND python_lib_output "${python_BUILD_DIR}/${python_lib_name}")
list (APPEND python_lib_output "${python_BUILD_DIR}-install/${python_lib_name}")

if (CMAKE_INSTALL_RPATH)
   set (python_INSTALL_RPATH "--rpath" "${CMAKE_INSTALL_RPATH}")
endif ()

add_custom_command (
   OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/python-lib" ${python_lib_output}
   COMMAND ${PYTHON_EXECUTABLE} setup.py
      # build_py just copies in order to combine it with config.py in one dir
      build_py --build-lib "${python_BUILD_DIR}"
      build_ext -f --rpath "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}" --build-lib "${python_BUILD_DIR}" --build-temp "${python_BUILD_DIR}-temp"
      # pseudo-install to compile and put into target location
      install_lib --install-dir ${python_LIB_DIR} --build-dir "${python_BUILD_DIR}"
   COMMAND ${PYTHON_EXECUTABLE} setup.py
      build_ext -f ${python_INSTALL_RPATH} --build-lib "${python_BUILD_DIR}-install" --build-temp "${python_BUILD_DIR}-temp"
   COMMAND ${CMAKE_COMMAND} -E touch "${CMAKE_CURRENT_BINARY_DIR}/python-lib"
   DEPENDS ${python_sources} Wrapper
   COMMENT "Compiling Python driver library"
)

add_custom_target (${PROJECT_NAME}-python-lib DEPENDS ${python_lib_output})

install (DIRECTORY "${python_LIB_DIR}/genevapylib"
   DESTINATION "lib/${python_dir}"
   FILES_MATCHING PATTERN "*.py" PATTERN "*.pyc"
)

install (DIRECTORY "${python_BUILD_DIR}-install/genevapylib"
   DESTINATION "lib/${python_dir}"
)

add_custom_target (${PROJECT_NAME}-python ALL)
add_dependencies (${PROJECT_NAME}-python ${PROJECT_NAME}-python-scripts)
add_dependencies (${PROJECT_NAME}-python ${PROJECT_NAME}-python-lib)

if (TARGET ${PROJECT_NAME}-lib)
   add_dependencies (${PROJECT_NAME}-lib ${PROJECT_NAME}-python-lib)
endif ()
