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
dd4hep_print("|++> OpticalSurfaces: ROOT version: ${ROOT_VERSION}")

if(NOT ${ROOT_VERSION} VERSION_GREATER_EQUAL 6.18.00)
  dd4hep_print("|++> Not building optical surface test")
  return()
endif()
dd4hep_print("|++> Building optical surface test")

#--------------------------------------------------------------------------
dd4hep_configure_output()

set(OpticalSurfaces_INSTALL ${CMAKE_INSTALL_DATADIR}/DD4hep/examples/OpticalSurfaces)
dd4hep_add_plugin(OpticalSurfacesExample SOURCES src/*.cpp
  USES DD4hep::DDCore DD4hep::DDCond ROOT::Core ROOT::Geom ROOT::GenVector ROOT::MathCore)
install(TARGETS OpticalSurfacesExample LIBRARY DESTINATION lib)
install(DIRECTORY compact scripts DESTINATION ${OpticalSurfaces_INSTALL} )
dd4hep_configure_scripts( OpticalSurfaces DEFAULT_SETUP WITH_TESTS)

#
#---Testing: Load ROOT GDMLMatrix objects from compact
#            and register them to the TGeoManager
dd4hep_add_test_reg( Surfaces_read_GDMLMatrices
  COMMAND    "${CMAKE_INSTALL_PREFIX}/bin/run_test_OpticalSurfaces.sh"
  EXEC_ARGS  geoPluginRun -volmgr -destroy 
  -compact file:${OpticalSurfaces_INSTALL}/compact/ReadGDMLMatrices.xml
  -plugin  DD4hep_Dump_GDMLTables
  REGEX_PASS "Successfully dumped 2 GDML tables with 42 elements."
  REGEX_FAIL " ERROR ;EXCEPTION;Exception"
  )
#
#---Testing: Load ROOT TGeoOpticalSurface objects from compact
#            and register them to the TGeoManager
dd4hep_add_test_reg( Surfaces_read_OpticalSurfaces
  COMMAND    "${CMAKE_INSTALL_PREFIX}/bin/run_test_OpticalSurfaces.sh"
  EXEC_ARGS  geoPluginRun -volmgr -destroy 
  -compact file:${OpticalSurfaces_INSTALL}/compact/ReadOpticalSurfaces.xml
  -plugin  DD4hep_Dump_GDMLTables
  -plugin  DD4hep_Dump_OpticalSurfaces
  REGEX_PASS "Successfully dumped 2 Optical surfaces."
  REGEX_FAIL " ERROR ;EXCEPTION;Exception"
  )
#
#---Testing: Load ROOT material properties and dump material
dd4hep_add_test_reg( Surfaces_read_MaterialProperties
  COMMAND    "${CMAKE_INSTALL_PREFIX}/bin/run_test_OpticalSurfaces.sh"
  EXEC_ARGS  geoPluginRun -volmgr -destroy -print INFO
  -compact file:${OpticalSurfaces_INSTALL}/compact/ReadMaterialProperties.xml
  -plugin DD4hep_MaterialTable -name Water
  REGEX_PASS "Property: SLOWCOMPONENT        \\[32 x 2\\] --> SLOWCOMPONENT__0x123aff00"
  REGEX_FAIL " ERROR ;EXCEPTION;Exception"
  )
#
# This plugins depends on the ROOT GDML readers. Hence, extra library
IF(TARGET ROOT::Gdml)
  #---Testing: Load OpNovice and dump it to GDML
  dd4hep_add_test_reg( Surfaces_OpNovice_write_gdml
    COMMAND    "${CMAKE_INSTALL_PREFIX}/bin/run_test_OpticalSurfaces.sh"
    EXEC_ARGS  geoPluginRun -volmgr -destroy 
    -compact file:${OpticalSurfaces_INSTALL}/compact/OpNovice.xml
    -plugin DD4hep_ROOTGDMLExtract -output OpNovice.gdml -path /world/BubbleDevice
    REGEX_PASS "File OpNovice.gdml saved"
    REGEX_FAIL " ERROR ;EXCEPTION;Exception"
  )
  #
  #
  if ( "${ROOT_VERSION}" VERSION_GREATER_EQUAL "6.19.01" )
    #
    #---Testing: Load GDML and dump volumes
    dd4hep_add_test_reg( Surfaces_OpNovice_read_gdml
      COMMAND    "${CMAKE_INSTALL_PREFIX}/bin/run_test_OpticalSurfaces.sh"
      EXEC_ARGS  geoPluginRun -volmgr -destroy 
      -compact file:${OpticalSurfaces_INSTALL}/compact/OpNovice_gdml.xml
      -print INFO -destroy -volmgr
      -plugin DD4hep_ROOTGDMLParse -input OpNovice.gdml -path /world/BubbleDevice
      -plugin DD4hep_VolumeDump
      DEPENDS    Surfaces_OpNovice_write_gdml
      REGEX_PASS "\\+\\+\\+ Checked 5 physical volume placements."
      REGEX_FAIL " ERROR ;EXCEPTION;Exception"
    )
  endif()
ELSE()
  dd4hep_print("ROOT does not include GDML, not building OpticalSurfaces GDML tests")
ENDIF()
#
