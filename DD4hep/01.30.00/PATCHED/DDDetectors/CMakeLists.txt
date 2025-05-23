#==========================================================================
#
#  AIDA Detector description implementation 
#--------------------------------------------------------------------------
# Copyright (C) Organisation europeenne pour la Recherche nucleaire (CERN)
# All rights reserved.
#
# For the licensing terms see $DD4hepINSTALL/LICENSE.
# For the list of contributors see $DD4hepINSTALL/doc/CREDITS.
#
#==========================================================================
#--------------------------------------------------------------------------
#  DDDetectors
#
#  Author:  M.Frank
#  Date:    13/11/2014
#
#--------------------------------------------------------------------------
SET_PROPERTY(DIRECTORY . PROPERTY PACKAGE_NAME DDDetectors)

dd4hep_add_plugin(DDDetectors 
  SOURCES src/*.cpp
  USES DD4hep::DDRec
  INCLUDES $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
)

set_target_properties(DDDetectors PROPERTIES VERSION ${DD4hep_VERSION} SOVERSION ${DD4hep_SOVERSION})
install(TARGETS DDDetectors EXPORT DD4hep
   DESTINATION ${CMAKE_INSTALL_LIBDIR}
  )
install(DIRECTORY compact DESTINATION ${CMAKE_INSTALL_DATADIR}/DD4hep/DDDetectors)
