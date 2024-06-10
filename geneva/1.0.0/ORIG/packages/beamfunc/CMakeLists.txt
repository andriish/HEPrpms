#-------------------------------------------------------------------------------
# Author(s):
#    Frank Tackmann
#
# Copyright:
#    Copyright (C) 2017 DESY
#
#    This file is part of the Geneva MC framework. Geneva is distributed under
#    the terms of the GNU General Public License version 3 (GPLv3), see the
#    COPYING file that comes with this distribution for details.
#    Please respect the academic usage guidelines in the GUIDELINES file.
#
# Description:
#    cmake file for beamfunc
#-------------------------------------------------------------------------------

# beamfunc packages requires LHAPDF
if (NOT geneva_USE_LHAPDF)
   set (msg "The beamfunc package requires the lhapdf library.")
   if (NOT geneva_enable_lhapdf)
      message (SEND_ERROR "EE ${msg}")
      message ("EE ${msg}")
      message (">> Please enable use of lhapdf by passing -Dgeneva_enable_lhapdf=on to cmake.")
   else ()
      message ("ww ${msg}")
   endif ()
endif ()

# variable where the user can select the beamfunc data dir
set (beamfunc_DATA_DIR "${${PROJECT_NAME}_DATA_DIR}/beamfunc")

# configure beamfunc.hpp
configure_file (
   ${CMAKE_CURRENT_SOURCE_DIR}/beamfunc.hpp.in
   ${CMAKE_BINARY_DIR}/include/beamfunc/beamfunc.hpp
)

# add our default compile flags
add_compile_options (${geneva_CXX_FLAGS})

# list of relevant source files
set (beamfunc_SOURCES
   TauCoeffsQuark.cpp
   InterpolateConv.cpp
)

add_library (beamfunc ${beamfunc_SOURCES})

set_target_properties (beamfunc PROPERTIES PREFIX "libGeneva-")

target_include_directories (beamfunc PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories (beamfunc PRIVATE ${CMAKE_BINARY_DIR}/include)

if (geneva_USE_LHAPDF)
   target_link_libraries (beamfunc PRIVATE lhapdf::lhapdf)
endif ()

install (TARGETS beamfunc DESTINATION lib)

#-------------------------------------------------------------------------------
# deal with beam function grids
#-------------------------------------------------------------------------------

# warn if beamfunc data doesn't appear to exist yet
message (STATUS "Using beamfunc data directory: ${beamfunc_DATA_DIR}")
foreach (grid IN LISTS beamfunc_PDF_SETS)
   if (EXISTS "${beamfunc_DATA_DIR}/${grid}_beamfunc/${grid}_fx/${grid}_fx_0000.dat")
      message (STATUS "Found beamfunc data files for ${grid}: ${beamfunc_DATA_DIR}/${grid}_beamfunc")
   else ()
      set (missing "${missing} ${grid}")
   endif ()
endforeach ()
if (missing)
   message ("ww Could NOT find beamfunc data files for:${missing}")
   message (">> Please specify the data directoy by passing -Dgeneva_DATA_DIR=path/to/data to cmake")
   message (">> Please specify the desired PDF sets by passing -Dbeamfunc_PDF_SETS=\"set1;set2;...\" to cmake")
   message (">> Use 'make beamfunc-install-data' to download and install missing data files.")
endif ()

set (URL "https://stash.desy.de/projects/GENEVA/repos/geneva-data/raw/beamfunc")

# write and generate scripts to install beamfunc grid files
function (write_install_data_script script_filename dest_share)
   foreach (grid IN LISTS ARGN)
      set (grids "${grids} ${grid}")
   endforeach ()
   file (WRITE ${script_filename}
"
message (STATUS \"Destination: ${dest_share}\")
message (STATUS \"PDF sets:${grids}\")
file (MAKE_DIRECTORY ${dest_share})
foreach (grid ${ARGN})
   set (file \"\${grid}_beamfunc.tar.gz\")
   if (NOT EXISTS \"${dest_share}/\${file}\")
      message (STATUS \"Downloading \${file}\")
      file (DOWNLOAD \"${URL}/\${file}\"  \"${dest_share}/\${file}\" SHOW_PROGRESS INACTIVITY_TIMEOUT 100 STATUS status LOG log)
      list (GET status 0 status_code)
      if (NOT status_code EQUAL 0)
         file (REMOVE \"${dest_share}/\${file}\")
         message (\"\${log}\")
         message (FATAL_ERROR \"Could NOT download \${file}\")
      endif()
   endif ()
   message (STATUS \"Extracting \${file}\")
   execute_process(
      COMMAND ${CMAKE_COMMAND} -E tar xzf ${dest_share}/\${file}
      WORKING_DIRECTORY ${dest_share}
   )
endforeach ()
"
   )
endfunction ()

function (write_uninstall_data_script script_filename dest_share)
   foreach (grid IN LISTS ARGN)
      set (grids "${grids} ${grid}")
   endforeach ()
   file (WRITE ${script_filename}
"
message (STATUS \"Destination: ${dest_share}\")
message (STATUS \"PDF sets:${grids}\")
foreach (grid ${ARGN})
   message (STATUS \"Removing: ${dest_share}/\${grid}_beamfunc\")
   file (REMOVE_RECURSE \"${dest_share}/\${grid}_beamfunc\")
   message (STATUS \"Removing: ${dest_share}/\${grid}_beamfunc.tar.gz\")
   file (REMOVE \"${dest_share}/\${grid}_beamfunc.tar.gz\")
endforeach ()
"
   )
endfunction ()

write_install_data_script (
   ${CMAKE_CURRENT_BINARY_DIR}/beamfunc-install-data.cmake
   ${beamfunc_DATA_DIR}
   ${beamfunc_PDF_SETS}
)

write_uninstall_data_script (
   ${CMAKE_CURRENT_BINARY_DIR}/beamfunc-uninstall-data.cmake
   ${beamfunc_DATA_DIR}
   ${beamfunc_PDF_SETS}
)

# add the custom target to install the beamfunc data files
add_custom_target (beamfunc-install-data
   COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/beamfunc-install-data.cmake
   COMMENT "Installing beamfunc data"
   VERBATIM
)

# add the custom target to uninstall the beamfunc data files
add_custom_target (beamfunc-uninstall-data
   COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/beamfunc-uninstall-data.cmake
   COMMENT "Uninstalling beamfunc data"
   VERBATIM
)
