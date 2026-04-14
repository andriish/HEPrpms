cmake_minimum_required(VERSION 3.11..4.0 FATAL_ERROR)
###########################Package@#####################################
project(TMDLIB LANGUAGES C CXX Fortran)
set(PROJECT_VERSION 2.2.15)
SET(TMDLIB_VERSION_MAJOR  2)
SET(TMDLIB_VERSION_MINOR  2)
SET(TMDLIB_VERSION_PATCH  15)
set(TMDLIB_VERSION "${TMDLIB_VERSION_MAJOR}.${TMDLIB_VERSION_MINOR}.${TMDLIB_VERSION_PATCH}")

set(CMAKE_VERBOSE_MAKEFILE ON)
if (NOT TMDLIB_CXX_STANDARD)
  set(TMDLIB_CXX_STANDARD 14)
endif()

###########Respect HFS, e.g. defines lib or lib64 when it is needed.####
include("GNUInstallDirs")
include(CheckCXXCompilerFlag)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules ${CMAKE_MODULE_PATH})
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    add_compile_options("$<$<CONFIG:Release>:-O3>")
    add_compile_options("$<$<CONFIG:Debug>:-O0;-g>")
endif()
###########################Options #####################################
option(TMDLIB_BUILD_DOCS           "Enables building of documentation" OFF)
option(TMDLIB_CREATE_VERSION_FILES "Enables creation of version files" OFF)
option(TMDLIB_ENABLE_PYTHON "Enables creation of python interfaces" ON)
option(TMDLIB_ENABLE_TEST        "Enables building of tests." ON)
option(TMDLIB_ENABLE_ROOT        "Enables building with ROOT." ON)

if (NOT TMDLIB_PYTHON_VERSIONS)
  set(TMDLIB_PYTHON_VERSIONS "2.X,3.X")
endif()

message(STATUS "TMDLIB: TMDLIB_BUILD_DOCS            ${TMDLIB_BUILD_DOCS}")
message(STATUS "TMDLIB: TMDLIB_CREATE_VERSION_FILES  ${TMDLIB_CREATE_VERSION_FILES}")
message(STATUS "TMDLIB: TMDLIB_ENABLE_PYTHON  ${TMDLIB_ENABLE_PYTHON}")

#### Modules ###########################################################
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules ${CMAKE_MODULE_PATH})

#### RPATH handling ####################################################
if(APPLE)
    set(CMAKE_MACOSX_RPATH 1)
else()
    # Linux: do NOT embed RPATH (required for RPM packaging)
    set(CMAKE_SKIP_RPATH TRUE)
    set(CMAKE_SKIP_BUILD_RPATH TRUE)
    set(CMAKE_SKIP_INSTALL_RPATH TRUE)
    set(CMAKE_INSTALL_RPATH "")
endif()


###################Windows##############################################
set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)

###################FLAGS################################################
SET(CMAKE_POSITION_INDEPENDENT_CODE ${OLD_CMAKE_POSITION_INDEPENDENT_CODE})

get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
message(STATUS "TMDLIB: Fortran_COMPILER_NAME=${Fortran_COMPILER_NAME}  CMAKE_Fortran_COMPILER_ID=${CMAKE_Fortran_COMPILER_ID}")
get_filename_component (CXX_COMPILER_NAME ${CMAKE_CXX_COMPILER} NAME)
message(STATUS "TMDLIB: CXX_COMPILER_NAME=${CXX_COMPILER_NAME}  CMAKE_CXX_COMPILER_ID=${CMAKE_CXX_COMPILER_ID}")
get_filename_component (C_COMPILER_NAME ${CMAKE_C_COMPILER} NAME)
message(STATUS "TMDLIB: C_COMPILER_NAME=${C_COMPILER_NAME}  CMAKE_C_COMPILER_ID=${CMAKE_C_COMPILER_ID}")
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set( CMAKE_Fortran_FLAGS  "${CMAKE_Fortran_FLAGS} -fno-automatic -fno-backslash -ffixed-line-length-132 -std=legacy")
#-g -fno-align-commons ?  
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  set( CMAKE_Fortran_FLAGS  "${CMAKE_Fortran_FLAGS}  -save -extend-source 132")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")
  set( CMAKE_Fortran_FLAGS  "${CMAKE_Fortran_FLAGS} -Mnobackslash -Mpreprocess -Mextend -Mnosave -Mnofree")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")
  set( CMAKE_Fortran_FLAGS  "${CMAKE_Fortran_FLAGS} -Mnobackslash -Mpreprocess -Mextend -Mnosave -Mnofree")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "SunPro")
  set( CMAKE_Fortran_FLAGS  "${CMAKE_Fortran_FLAGS}  ")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "XL")
  set( CMAKE_Fortran_FLAGS  "${CMAKE_Fortran_FLAGS} -qfixed=256  -qnosave -qextname")
else()
  message(STATUS "TMDLIB examples: Don't know how to set Fortran FLAGS for  ${Fortran_COMPILER_NAME} with ID ${Fortran_COMPILER_ID}")
endif ()


######Mandatory PACKAGES################################################
find_package(LHAPDF REQUIRED)
message(STATUS "TMDLIB: LHAPDF_VERSION=${LHAPDF_VERSION} LHAPDF_LIBRARIES=${LHAPDF_LIBRARIES} LHAPDF_INCLUDE_DIRS=${LHAPDF_INCLUDE_DIRS}")


find_package(GSL REQUIRED)
message(STATUS "TMDLIB: GSL_VERSION=${GSL_VERSION} GSL_LIBRARIES=${GSL_LIBRARIES} GSL_INCLUDE_DIRS=${GSL_INCLUDE_DIRS}")
if (TMDLIB_ENABLE_ROOT)
find_package(ROOT QUIET)
endif()

if(NOT ROOT_FOUND)
    message(STATUS "ROOT package not found, continuing without ROOT support. TMDPlotter cannot be built")
else()
    message(STATUS "ROOT package found: ${ROOT_VERSION}")
endif()
if (ROOT_FOUND)
  include(${ROOT_USE_FILE})
  message(STATUS "ROOT package found ROOT_CXX_STANDARD: ${ROOT_CXX_STANDARD}")
  if(ROOT_CXX_STANDARD)
    set(TMDLIB_CXX_STANDARD ${ROOT_CXX_STANDARD})
    set(CMAKE_CXX_STANDARD ${TMDLIB_CXX_STANDARD})
    set(CMAKE_CXX_STANDARD_REQUIRED ON)
    set(CMAKE_CXX_EXTENSIONS OFF)
    message(STATUS "TMDLIB: ROOT_VERSION=${ROOT_VERSION} ROOT_LIBRARIES=${ROOT_LIBRARIES} ROOT_INCLUDE_DIRS=${ROOT_INCLUDE_DIRS}")
  endif()
endif()

######OPTIONAL PACKAGES#################################################


if (TMDLIB_BUILD_DOCS)
  find_package(LATEX COMPONENTS PDFLATEX MAKEINDEX)
  message(STATUS "TMDLIB: LATEX_FOUND=${LATEX_FOUND} PDFLATEX_COMPILER=${PDFLATEX_COMPILER} MAKEINDEX_COMPILER=${MAKEINDEX_COMPILER}")
endif()

#Create version files###################################################

  file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/include/TMDlibPDFpath.h"  "    std::string pdfpath= \"${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/tmdlib/\" ;\n")
  file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/include/TMDlibPDFpath.inc"  "         character*512 PDFPATH \n         DATA PDFPATH/\"${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/tmdlib/\" / \n")
  file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/include/TMDlibvers-tex.inc" " ${TMDLIB_VERSION}\n")
  


set(TMDLIB_SOURCES   
			TMDhandler.cc TMDalphas.cc TMDutils.cc 
			allFlavupdf/allFlavuPDF.cc allFlavupdf/allFlavuPDF_old.cc allFlavupdf/Cdhfint.cc allFlavupdf/allFlavuPDFn.cc 
                  allFlavupdf/allFlavuPDFew.cc allFlavupdf/ccfm_gluon.cc 
                  allFlavupdf/blueml.f 
                  TMDlibfortran.cc 
			gbw/gbwuPDF.cc 
			sbrs/polint.cc sbrs/sbrsPDF.cc 
			ks/ksPDF.cc ks/UnintegratedGluonKS.cc ks/InterpolationKS.cc ks/ksDLCPDF.cc ks/ksBHKSPDF.cc 
                  EKMP/tmd_ME.cc EKMP/alphas.f EKMP/kernel_g.f EKMP/mstwpdf.f EKMP/unpolarized.f 
			yaml-cpp-yaml-cpp-0.6.0/src/binary.cpp          yaml-cpp-yaml-cpp-0.6.0/src/emitterstate.cpp   
                  yaml-cpp-yaml-cpp-0.6.0/src/node_data.cpp       yaml-cpp-yaml-cpp-0.6.0/src/parser.cpp  
                  yaml-cpp-yaml-cpp-0.6.0/src/simplekey.cpp 	yaml-cpp-yaml-cpp-0.6.0/src/convert.cpp  
                  yaml-cpp-yaml-cpp-0.6.0/src/emitterutils.cpp    yaml-cpp-yaml-cpp-0.6.0/src/nodebuilder.cpp 
                  yaml-cpp-yaml-cpp-0.6.0/src/regex_yaml.cpp      yaml-cpp-yaml-cpp-0.6.0/src/singledocparser.cpp 
                  yaml-cpp-yaml-cpp-0.6.0/src/directives.cpp      yaml-cpp-yaml-cpp-0.6.0/src/exceptions.cpp     
                  yaml-cpp-yaml-cpp-0.6.0/src/nodeevents.cpp      yaml-cpp-yaml-cpp-0.6.0/src/scanner.cpp    
                	yaml-cpp-yaml-cpp-0.6.0/src/stream.cpp		yaml-cpp-yaml-cpp-0.6.0/src/emit.cpp 
                  yaml-cpp-yaml-cpp-0.6.0/src/exp.cpp             yaml-cpp-yaml-cpp-0.6.0/src/null.cpp 
                  yaml-cpp-yaml-cpp-0.6.0/src/scanscalar.cpp      yaml-cpp-yaml-cpp-0.6.0/src/tag.cpp 
                  yaml-cpp-yaml-cpp-0.6.0/src/emitfromevents.cpp  yaml-cpp-yaml-cpp-0.6.0/src/memory.cpp 
                  yaml-cpp-yaml-cpp-0.6.0/src/ostream_wrapper.cpp yaml-cpp-yaml-cpp-0.6.0/src/scantag.cpp 
                  yaml-cpp-yaml-cpp-0.6.0/src/emitter.cpp         yaml-cpp-yaml-cpp-0.6.0/src/node.cpp 
                  yaml-cpp-yaml-cpp-0.6.0/src/parse.cpp           yaml-cpp-yaml-cpp-0.6.0/src/scantoken.cpp 
       		Pavia/Pavia.cc
)
list(TRANSFORM TMDLIB_SOURCES PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/src/")


set(libapfelxx_la_SOURCES 
			evolution/alphaqcd.cc      evolution/evolutionbasisqcd.cc          evolution/splittingfunctionspol_sl.cc 
			evolution/alphaqed.cc      evolution/lhtoypdfs.cc                  evolution/splittingfunctionstrans_sl.cc 
			evolution/betaqcd.cc       evolution/matchingbasisqcd.cc           evolution/splittingfunctionstrans_tl.cc 
			evolution/betaqed.cc       evolution/matchingconditions_sl.cc      evolution/splittingfunctionsunp_sl.cc 
			evolution/dglapbuilder.cc  evolution/matchingconditions_tl.cc      evolution/splittingfunctionsunp_tl.cc 
			gpds/gpdbuilder.cc          gpds/gpdevolutionkernels.cc 
			interface/initialiseevolution.cc 
			kernel/convolutionmap.cc       kernel/grid.cc                 kernel/matchedevolution.cc     kernel/qgrid.cc
                  kernel/tabulateobject.cc 
			kernel/dglap.cc                kernel/hplog.f                 kernel/matrix.cc               kernel/rotations.cc            
                  kernel/tools.cc 
			kernel/distribution.cc         kernel/integrator.cc           kernel/messages.cc             kernel/set.cc 
			kernel/doubleobject.cc         kernel/interpolator.cc         kernel/observable.cc           kernel/specialfunctions.cc 
			kernel/expression.cc           kernel/lagrangeinterpolator.cc kernel/operator.cc             kernel/subgrid.cc 
			structurefunctions/disbasis.cc                            structurefunctions/structurefunctionbuilder.cc 
			structurefunctions/hqcoef.f                                 structurefunctions/zeromasscoefficientfunctionspol_sl.cc 
			structurefunctions/massivecoefficientfunctionsunp_sl.cc     structurefunctions/zeromasscoefficientfunctionsunp_sl.cc 
			structurefunctions/massivezerocoefficientfunctionsunp_sl.cc structurefunctions/zeromasscoefficientfunctionsunp_tl.cc 
			tmds/doubleexponentialquadrature.cc tmds/kcs.cc                         tmds/tmdbuilder.cc 
			tmds/gammaf.cc                      tmds/matchingfunctionsff.cc         tmds/twobodyphasespace.cc 
			tmds/gammak.cc                      tmds/matchingfunctionspdf.cc 
			tmds/hardfactors.cc                 tmds/ogataquadrature.cc )

list(TRANSFORM libapfelxx_la_SOURCES PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/apfelxx/src/")


add_library(apfelxx SHARED ${libapfelxx_la_SOURCES})
target_include_directories(apfelxx PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/include/ ${PROJECT_SOURCE_DIR}/apfelxx/include ${PROJECT_SOURCE_DIR}/apfelxx/inc ${PROJECT_SOURCE_DIR}/include ${PROJECT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include ${GSL_INCLUDE_DIRS} )
target_compile_definitions(apfelxx PRIVATE -DXMLDIR=\"xxx\")
target_compile_definitions(apfelxx PRIVATE -DVERSION=1)
target_link_libraries(apfelxx PRIVATE ${GSL_LIBRARIES} )
install(TARGETS apfelxx DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT libs)

add_library(TMDlib SHARED ${TMDLIB_SOURCES})
target_include_directories(TMDlib PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/include/  ${CMAKE_CURRENT_SOURCE_DIR}/include/ ${CMAKE_CURRENT_SOURCE_DIR}/ ${CMAKE_CURRENT_BINARY_DIR}/ ${CMAKE_CURRENT_BINARY_DIR}/include/ ${LHAPDF_INCLUDE_DIRS} ${PROJECT_SOURCE_DIR}/apfelxx/include ${PROJECT_SOURCE_DIR}/apfelxx/inc ${PROJECT_SOURCE_DIR}/include ${PROJECT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include ${GSL_INCLUDE_DIRS} )
target_compile_definitions(TMDlib PRIVATE -DXMLDIR=\"xxx\")
target_link_libraries(TMDlib PRIVATE apfelxx ${LHAPDF_LIBRARIES} ${GSL_LIBRARIES})
install(TARGETS TMDlib DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT libs)

#add_executable(mwg  amend/main.cc)
#target_include_directories(mwg  PRIVATE amend)
if (ROOT_FOUND)
  add_executable(TMDplot  ${CMAKE_CURRENT_SOURCE_DIR}/examples-c++/TMDplotter.cc) 
  target_include_directories(TMDplot PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/include/  ${CMAKE_CURRENT_SOURCE_DIR}/include/ ${CMAKE_CURRENT_BINARY_DIR}/include ${LHAPDF_INCLUDE_DIRS} ${PROJECT_SOURCE_DIR}/apfelxx/include ${PROJECT_SOURCE_DIR}/apfelxx/inc ${PROJECT_SOURCE_DIR}/include ${PROJECT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include ${GSL_INCLUDE_DIRS} ${ROOT_INCLUDE_DIRS} )
  target_link_libraries(TMDplot PRIVATE TMDlib apfelxx ${LHAPDF_LIBRARIES} ${GSL_LIBRARIES} ${ROOT_LIBRARIES} )
  install(TARGETS TMDplot DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT binary)
endif()
add_executable(TMDlib_test  ${CMAKE_CURRENT_SOURCE_DIR}/examples-c++/TMDlib_test.cc) 
target_include_directories(TMDlib_test PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/include/  ${CMAKE_CURRENT_SOURCE_DIR}/include/ ${CMAKE_CURRENT_BINARY_DIR}/include ${LHAPDF_INCLUDE_DIRS} ${PROJECT_SOURCE_DIR}/apfelxx/include ${PROJECT_SOURCE_DIR}/apfelxx/inc ${PROJECT_SOURCE_DIR}/include ${PROJECT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include ${GSL_INCLUDE_DIRS} ${ROOT_INCLUDE_DIRS} )
target_link_libraries(TMDlib_test PRIVATE TMDlib apfelxx ${LHAPDF_LIBRARIES} ${GSL_LIBRARIES} ${ROOT_LIBRARIES} )
install(TARGETS TMDlib_test DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT binary)

if (ROOT_FOUND)
  add_executable(TMDplotter  ${CMAKE_CURRENT_SOURCE_DIR}/TMDplotter/tmdplotter.cc TMDplotter/Curve.cc TMDplotter/tmdexception.cc )
  target_include_directories(TMDplotter PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/include/  ${CMAKE_CURRENT_SOURCE_DIR}/include/  ${CMAKE_CURRENT_BINARY_DIR}/include ${LHAPDF_INCLUDE_DIRS} ${PROJECT_SOURCE_DIR}/apfelxx/include ${PROJECT_SOURCE_DIR}/apfelxx/inc ${PROJECT_SOURCE_DIR}/include ${PROJECT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include ${GSL_INCLUDE_DIRS} ${ROOT_INCLUDE_DIRS} )
  target_link_libraries(TMDplotter PRIVATE TMDlib apfelxx ${LHAPDF_LIBRARIES} ${GSL_LIBRARIES} ${ROOT_LIBRARIES} )
  install(TARGETS TMDplotter DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT binary)
endif()
add_executable(TMDlib_Fortran  ${CMAKE_CURRENT_SOURCE_DIR}/examples-fortran/TMDlib_Fortran.F)
target_include_directories(TMDlib_Fortran PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/include ${LHAPDF_INCLUDE_DIRS} ${PROJECT_SOURCE_DIR}/apfelxx/include ${PROJECT_SOURCE_DIR}/apfelxx/inc ${PROJECT_SOURCE_DIR}/include ${PROJECT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include ${GSL_INCLUDE_DIRS} ${ROOT_INCLUDE_DIRS} )
target_link_libraries(TMDlib_Fortran PRIVATE TMDlib apfelxx ${LHAPDF_LIBRARIES} ${GSL_LIBRARIES} ${ROOT_LIBRARIES} )
install(TARGETS TMDlib_Fortran DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT binary)


add_executable(TMDlib_Fortran_many  ${CMAKE_CURRENT_SOURCE_DIR}/examples-fortran/TMDlib_Fortran_many.F)
target_include_directories(TMDlib_Fortran_many PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/include ${LHAPDF_INCLUDE_DIRS} ${PROJECT_SOURCE_DIR}/apfelxx/include ${PROJECT_SOURCE_DIR}/apfelxx/inc ${PROJECT_SOURCE_DIR}/include ${PROJECT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include ${GSL_INCLUDE_DIRS} ${ROOT_INCLUDE_DIRS} )
target_link_libraries(TMDlib_Fortran_many PRIVATE TMDlib apfelxx ${LHAPDF_LIBRARIES} ${GSL_LIBRARIES} ${ROOT_LIBRARIES} )
install(TARGETS TMDlib_Fortran_many DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT binary)


if(TMDLIB_ENABLE_TEST)
  ENABLE_TESTING()

  find_program( MEMORYCHECK_COMMAND valgrind )
  if(MEMORYCHECK_COMMAND)
    message(STATUS "TMDlib test: valgrind package found in ${MEMORYCHECK_COMMAND}.  memory check tests enabled.")
    set( MEMORYCHECK_COMMAND_OPTIONS "--show-leak-kinds=all --trace-children=yes --leak-check=full --error-exitcode=1 --default-suppressions=yes --gen-suppressions=all" )
    if (${CMAKE_CXX_COMPILER_ID} MATCHES "Intel")
       file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/intelTMDlib.supp" "{\n<TMDlib_at_Linux_with_Intel_1>\nMemcheck:Addr32\nfun:__intel_*\n}\n{\n<TMDlib_at_Linux_with_Intel_2>\nMemcheck:Cond\nfun:*strchr\n}\n"  )
       set( MEMORYCHECK_COMMAND_OPTIONS  "${MEMORYCHECK_COMMAND_OPTIONS} --suppressions=${CMAKE_CURRENT_BINARY_DIR}/IntelTMDlib.supp")
    endif()
  else()
    message(STATUS "TMDlib test: valgrind package not found or disabled.  memory check tests disabled.")
  endif()
##  add_executable(TMDlib_test  ${CMAKE_CURRENT_SOURCE_DIR}/examples-c++/TMDlib_test.cc)
##  target_include_directories(TMDlib_test PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/include/  ##${CMAKE_CURRENT_SOURCE_DIR}/include/ ${CMAKE_CURRENT_BINARY_DIR}/include ${LHAPDF_INCLUDE_DIRS} ##${PROJECT_SOURCE_DIR}/apfelxx/include ${PROJECT_SOURCE_DIR}/apfelxx/inc ${PROJECT_SOURCE_DIR}/include ##${PROJECT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include ${GSL_INCLUDE_DIRS}  )
##  target_link_libraries(TMDlib_test PRIVATE TMDlib apfelxx ${LHAPDF_LIBRARIES} ${GSL_LIBRARIES})
  add_test(NAME  test1  COMMAND  ${CMAKE_CURRENT_BINARY_DIR}/TMDlib_test ${CMAKE_SOURCE_DIR}/data/ Blueml)
#  add_executable(TMDlib_testF  ${CMAKE_CURRENT_SOURCE_DIR}/examples-fortran/TMDlib_Fortran.F)
#  target_link_libraries(TMDlib_testF PRIVATE TMDlib apfelxx ${LHAPDF_LIBRARIES} ${GSL_LIBRARIES})
#  add_test(NAME  test1f  COMMAND  ${CMAKE_CURRENT_BINARY_DIR}/TMDlib_testF)
#  SET_TESTS_PROPERTIES( test1f PROPERTIES ENVIRONMENT "TMDLIBPATH=${CMAKE_SOURCE_DIR}/data/" )
#  if(MEMORYCHECK_COMMAND)
#    set(memcheck_command "${MEMORYCHECK_COMMAND} ${MEMORYCHECK_COMMAND_OPTIONS}")
#    separate_arguments(memcheck_command)
#    add_test( NAME mem_test1 COMMAND  ${memcheck_command} ${CMAKE_CURRENT_BINARY_DIR}/TMDlib_test ${CMAKE_SOURCE_DIR}/data/ Blueml)
#    add_test( NAME mem_test1f COMMAND  ${memcheck_command} ${CMAKE_CURRENT_BINARY_DIR}/TMDlib_testF)
#    SET_TESTS_PROPERTIES( mem_test1f PROPERTIES ENVIRONMENT "TMDLIBPATH=${CMAKE_SOURCE_DIR}/data/" )
#  endif()
endif()


#plugins/python/src
## MANUAL  #############################################################
if (TMDLIB_BUILD_DOCS AND LATEX_FOUND AND LATEX_PDFLATEX_FOUND AND LATEX_MAKEINDEX_FOUND)
  set( manual_sources 
       ${PROJECT_SOURCE_DIR}/manual/TMDlib2.tex 
#       ${PROJECT_SOURCE_DIR}/manual/TMDLIB2.bbl 
       ${PROJECT_SOURCE_DIR}/manual/mystyle-new.sty 
       ${PROJECT_SOURCE_DIR}/manual/mcite.sty
       ${PROJECT_SOURCE_DIR}/manual/PB-HERAPDF-collinear-mu100-new.pdf
       ${PROJECT_SOURCE_DIR}/manual/PV-TMD-up-mu100.pdf
       ${PROJECT_SOURCE_DIR}/manual/TMDplotter-screenshot-kt.pdf
       ${PROJECT_SOURCE_DIR}/manual/PBset2-TMD-up-mu100-with-q0.pdf
       ${PROJECT_SOURCE_DIR}/manual/PV17-MMHE2014-collinear-new.pdf
       ${PROJECT_SOURCE_DIR}/manual/TMDplotter-screenshot-luminosity.pdf
       ${PROJECT_SOURCE_DIR}/manual/TMDlibvers-tex.inc

     )
     file(COPY ${manual_sources} DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/manual)
     ADD_CUSTOM_TARGET( latexcompileall
                       COMMAND ${PDFLATEX_COMPILER} TMDLIB2
                       COMMAND ${PDFLATEX_COMPILER} TMDLIB2
                       COMMAND ${PDFLATEX_COMPILER} TMDLIB2
                       COMMENT "Creating manual "
                       WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/manual
                       DEPENDS ${PROJECT_SOURCE_DIR}/manual/TMDlib2.tex
                      )
     ADD_CUSTOM_TARGET(all-formats ALL) # Entry point of execution.
     ADD_DEPENDENCIES( all-formats latexcompileall)
     install(FILES ${CMAKE_CURRENT_BINARY_DIR}/manual/TMDlib2.pdf DESTINATION  ${CMAKE_INSTALL_MANDIR} COMPONENT man)
endif()
### DATA ###############################################################
set(gbwlight   
    ${PROJECT_SOURCE_DIR}/data/GBWlight/GBWlight.info  
  )

set(gbwcharm   
    ${PROJECT_SOURCE_DIR}/data/GBWcharm/GBWcharm.info  
  )
set(Blueml   
    ${PROJECT_SOURCE_DIR}/data/Blueml/Blueml.info 
  )
set(Template  
    ${PROJECT_SOURCE_DIR}/data/Template.info 
    ${PROJECT_SOURCE_DIR}/data/tmdpdfsets.index 
    ${PROJECT_SOURCE_DIR}/data/config_tmdplotter.ini
  )
install(FILES ${gbwlight} DESTINATION  ${CMAKE_INSTALL_DATADIR}/tmdlib/GBWlight COMPONENT doc)
install(FILES ${gbwcharm} DESTINATION  ${CMAKE_INSTALL_DATADIR}/tmdlib/GBWcharm COMPONENT doc)
install(FILES ${Blueml} DESTINATION  ${CMAKE_INSTALL_DATADIR}/tmdlib/Blueml COMPONENT doc)

install(FILES ${Template} DESTINATION  ${CMAKE_INSTALL_DATADIR}/tmdlib COMPONENT doc)

install(DIRECTORY include/ DESTINATION ${CMAKE_INSTALL_INCLUDEDIR} COMPONENT devel 
                                                                   PATTERN *.h  
                                                                   PATTERN *path* EXCLUDE
                                                                   PATTERN *Make* EXCLUDE
                                                                   )

install(FILES "${CMAKE_CURRENT_BINARY_DIR}/include/TMDlibPDFpath.h" DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/tmdlib/ )
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/include/TMDlibPDFpath.inc" DESTINATION  ${CMAKE_INSTALL_INCLUDEDIR}/tmdlib/ )
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/include/TMDlibvers-tex.inc" DESTINATION  ${CMAKE_INSTALL_INCLUDEDIR}/tmdlib/)

install(FILES     ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/yaml.h
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/parser.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/dll.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/noncopyable.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/emitter.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/binary.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/emitterdef.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/emittermanip.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/null.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/ostream_wrapper.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/emitterstyle.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/stlemitter.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/exceptions.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/mark.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/traits.h  
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/anchor.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/eventhandler.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/emitfromevents.h 
                  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/yaml-cpp/ COMPONENT devel )

install(FILES     ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/node/node.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/node/ptr.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/node/type.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/node/iterator.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/node/impl.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/node/convert.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/node/parse.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/node/emit.h
                  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/yaml-cpp/node/ COMPONENT devel )

install(FILES     ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/node/detail/bool_type.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/node/detail/iterator_fwd.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/node/detail/iterator.h  
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/node/detail/node_iterator.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/node/detail/memory.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/node/detail/node.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/node/detail/node_ref.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/node/detail/node_data.h 
                  ${CMAKE_CURRENT_SOURCE_DIR}/src/yaml-cpp-yaml-cpp-0.6.0/include/yaml-cpp/node/detail/impl.h 
                  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/yaml-cpp/node/detail COMPONENT devel )


set(PACKAGE_VERSION ${TMDLIB_VERSION})
set(prefix          ${CMAKE_INSTALL_PREFIX})
set(datarootdir     ${CMAKE_INSTALL_DATADIR})
set(datadir         ${CMAKE_INSTALL_DATADIR})
set(exec_prefix     "\$\{prefix\}")
set(includedir      "\$\{prefix\}/include/tmdlib/")
set(libdir          "/${CMAKE_INSTALL_LIBDIR}")
set(datadir         "\$\{prefix\}/${CMAKE_INSTALL_DATADIR}")
set(datarootdir     "\$\{prefix\}/share")

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/TMDlib-config.in ${CMAKE_CURRENT_BINARY_DIR}/TMDlib-config @ONLY)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/TMDlib-config DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT binary  PERMISSIONS WORLD_EXECUTE WORLD_READ OWNER_READ OWNER_WRITE OWNER_EXECUTE)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/TMDlib-getdata.in ${CMAKE_CURRENT_BINARY_DIR}/TMDlib-getdata @ONLY)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/TMDlib-getdata DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT binary PERMISSIONS WORLD_EXECUTE WORLD_READ OWNER_READ OWNER_WRITE OWNER_EXECUTE )

########################################################################

include(CMakePackageConfigHelpers)
include(CMakePackageConfigHelpers)

set(CONFIG_INSTALL_DIR ${CMAKE_INSTALL_DATADIR}/tmdlib/cmake)

configure_package_config_file(cmake/Templates/TMDlibConfig.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/share/tmdlib/cmake/TMDlibConfig.cmake
  INSTALL_DESTINATION ${CONFIG_INSTALL_DIR}
  PATH_VARS CMAKE_INSTALL_INCLUDEDIR
            CMAKE_INSTALL_LIBDIR)

write_basic_package_version_file(${CMAKE_CURRENT_BINARY_DIR}/share/tmdlib/cmake/TMDlibConfig-version.cmake COMPATIBILITY SameMajorVersion VERSION ${PROJECT_VERSION})


install(FILES ${CMAKE_CURRENT_BINARY_DIR}/share/tmdlib/cmake/TMDlibConfig.cmake
              ${CMAKE_CURRENT_BINARY_DIR}/share/tmdlib/cmake/TMDlibConfig-version.cmake
        DESTINATION ${CONFIG_INSTALL_DIR} COMPONENT devel)


#Python bindings
if(TMDLIB_ENABLE_PYTHON)
  add_subdirectory(python)
endif()

find_program (ASTYLE NAMES astyle)
if (ASTYLE)
  message(STATUS "TMDLIB: Developers only: astyle found in ${ASTYLE}. Formating of C++ code is possible.")
  ADD_CUSTOM_TARGET(beauty
    COMMAND ${ASTYLE}  --keep-one-line-blocks
    examples-*/*cc
    amend/*cc
    src/*cc
    src/*/*cc
    include/*h
    include/*/*h
    TMDplotter/*h
    TMDplotter/*cc
    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
  )
endif()
