#=================================================================================
#
#  AIDA Detector description implementation 
#---------------------------------------------------------------------------------
# Copyright (C) Organisation europeenne pour la Recherche nucleaire (CERN)
# All rights reserved.
#
# For the licensing terms see $DD4hepINSTALL/LICENSE.
# For the list of contributors see $DD4hepINSTALL/doc/CREDITS.
#
#=================================================================================

dd4hep_print("|++> Will build DDParsers")


# set the default job pool to use, is unset below, although this only applies for this directory
SET(CMAKE_JOB_POOL_COMPILE HIGH_MEM_POOL)

file(GLOB DDParsers_SOURCES src/Parsers/*.cpp src/Evaluator/*.cpp)
file(GLOB DDParsers_SPIRIT_SOURCES src/Spirit/*.cpp)

add_library(DDParsers ${DDParsers_SOURCES} ${DDParsers_SPIRIT_SOURCES})
add_library(DD4hep::DDParsers ALIAS DDParsers)

target_compile_definitions(DDParsers INTERFACE BOOST_SPIRIT_USE_PHOENIX_V3)
IF( DD4HEP_USE_GEANT4_UNITS )
  target_compile_definitions(DDParsers PUBLIC DD4HEP_USE_GEANT4_UNITS=1)
ENDIF()

target_include_directories(DDParsers
  PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
  )

target_link_libraries(DDParsers PUBLIC ROOT::Core ROOT::GenVector Boost::boost ${XML_LIBRARIES})

set_target_properties(DDParsers PROPERTIES VERSION ${DD4hep_VERSION} SOVERSION ${DD4hep_SOVERSION})
install(DIRECTORY include/Parsers include/Evaluator DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
install(TARGETS DDParsers ${OPTIONAL_TARGETS} EXPORT DD4hep
   DESTINATION ${CMAKE_INSTALL_LIBDIR})
# Unset job pool
SET(CMAKE_JOB_POOL_COMPILE)
