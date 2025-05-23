#==========================================================================
#  AIDA Detector description implementation 
#--------------------------------------------------------------------------
# Copyright (C) Organisation europeenne pour la Recherche nucleaire (CERN)
# All rights reserved.
#
# For the licensing terms see $DD4hepINSTALL/LICENSE.
# For the list of contributors see $DD4hepINSTALL/doc/CREDITS.
#
#==========================================================================
cmake_minimum_required(VERSION 3.14 FATAL_ERROR)

IF(NOT TARGET DD4hep::DDCore)
  find_package ( DD4hep REQUIRED )
  include ( ${DD4hep_DIR}/cmake/DD4hep.cmake )
  include ( ${DD4hep_DIR}/cmake/DD4hepBuild.cmake )
  dd4hep_configure_output()
ENDIF()

dd4hep_set_compiler_flags()
#==========================================================================
#---DDCodex plugin library -------------------------------------------------------
dd4hep_add_plugin(DDCodexPlugins
  SOURCES src/*.cpp
  USES    DD4hep::DDCore ROOT::Core ROOT::Geom ROOT::GenVector
  )
install(TARGETS DDCodexPlugins DESTINATION lib)
#
#
install(DIRECTORY compact python eve DESTINATION ${CMAKE_INSTALL_DATADIR}/DD4hep/examples/DDCodex )
install(FILES Upgrade.root DESTINATION ${CMAKE_INSTALL_DATADIR}/DD4hep/examples/DDCodex )
#--------------------------------------------------------------------------
#
dd4hep_configure_scripts ( DDCodex DEFAULT_SETUP WITH_TESTS )
