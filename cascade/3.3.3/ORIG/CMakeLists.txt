cmake_minimum_required(VERSION 3.1.0 FATAL_ERROR)
###########################Package@#####################################
project(cascade LANGUAGES C CXX Fortran )
set(PROJECT_VERSION 3.2.05)
SET(CASCADE_VERSION 3.2.05)
SET(CASCADE_VERSION_MAJOR  3)
SET(CASCADE_VERSION_MINOR  2)
SET(CASCADE_VERSION_PATCH  5)

set(CMAKE_VERBOSE_MAKEFILE ON)

###########Respect HFS, e.g. defines lib or lib64 when it is needed.####
include("GNUInstallDirs")

###########################Options #####################################
option(CASCADE_BUILD_DOCS           "Enables building of documentation" ON)
option(CASCADE_CREATE_VERSION_FILES "Enables creation of version files" OFF)
option(CASCADE_PYTHIA8              "Enables Pythia8 routines" ON)
option(CASCADE_HEPMC2               "Enables HepMC2 routines" OFF)
option(CASCADE_HEPMC3               "Enables HepMC3 routines" ON)
option(CASCADE_RIVET                "Enables RIVET routines" ON)
option(CASCADE_TMDLIB               "Enables TMDLIB routines" ON)
option(CASCADE_TMDLIB_EW            "Enables TMDLIB_EW routines" ON)
option(CASCADE_ENABLE_TESTS         "Enables tests" ON)
message(STATUS "CASCADE: CASCADE_BUILD_DOCS            ${CASCADE_BUILD_DOCS}")
message(STATUS "CASCADE: CASCADE_CREATE_VERSION_FILES  ${CASCADE_CREATE_VERSION_FILES}")
message(STATUS "CASCADE: CASCADE_PYTHIA8               ${CASCADE_PYTHIA8}")
message(STATUS "CASCADE: CASCADE_HEPMC2                ${CASCADE_HEPMC2}")
message(STATUS "CASCADE: CASCADE_HEPMC3                ${CASCADE_HEPMC3}")
message(STATUS "CASCADE: CASCADE_RIVET                 ${CASCADE_RIVET}")
message(STATUS "CASCADE: CASCADE_TMDLIB                ${CASCADE_TMDLIB}")
message(STATUS "CASCADE: CASCADE_TMDLIB_EW             ${CASCADE_TMDLIB_EW}")
message(STATUS "CASCADE: CASCADE_ENABLE_TESTS          ${CASCADE_ENABLE_TESTS}")

if(CMAKE_SYSTEM_NAME MATCHES Darwin)
  if (NOT CMAKE_MACOSX_RPATH) 
    set(CMAKE_MACOSX_RPATH 1)
  endif()
  message(STATUS "RAPGAP: CMAKE_MACOSX_RPATH            ${CMAKE_MACOSX_RPATH}. Set by default to 1. Set to 0 to disable RPATH in the build executables.")
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
message(STATUS "CASCADE: Fortran_COMPILER_NAME=${Fortran_COMPILER_NAME}  CMAKE_Fortran_COMPILER_ID=${CMAKE_Fortran_COMPILER_ID}")
get_filename_component (CXX_COMPILER_NAME ${CMAKE_CXX_COMPILER} NAME)
message(STATUS "CASCADE: CXX_COMPILER_NAME=${CXX_COMPILER_NAME}  CMAKE_CXX_COMPILER_ID=${CMAKE_CXX_COMPILER_ID}")
get_filename_component (C_COMPILER_NAME ${CMAKE_C_COMPILER} NAME)
message(STATUS "CASCADE: C_COMPILER_NAME=${C_COMPILER_NAME}  CMAKE_C_COMPILER_ID=${CMAKE_C_COMPILER_ID}")
set( CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} -std=c++1y")
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set( CMAKE_Fortran_FLAGS  "${CMAKE_Fortran_FLAGS} -fno-automatic -fno-backslash -ffixed-line-length-132 -std=legacy")
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
elseif()
  message(STATUS "CASCADE examples: Don't know how to set Fortran FLAGS for  ${Fortran_COMPILER_NAME} with ID ${Fortran_COMPILER_ID}")
endif ()
if(CMAKE_SYSTEM_NAME MATCHES Darwin)
  set(CMAKE_SHARED_LIBRARY_CREATE_Fortran_FLAGS "${CMAKE_SHARED_LIBRARY_CREATE_Fortran_FLAGS} -flat_namespace -single_module -undefined dynamic_lookup")
else()
endif()

######Mandatory External PACKAGES################################################
find_package(ZLIB REQUIRED)
message(STATUS "CASCADE: ZLIB_VERSION_STRING=${ZLIB_VERSION_STRING} ZLIB_LIBRARIES=${ZLIB_LIBRARIES} ZLIB_INCLUDE_DIRS=${ZLIB_INCLUDE_DIRS}")
find_package(GSL REQUIRED)
message(STATUS "CASCADE: GSL_VERSION=${GSL_VERSION} GSL_LIBRARIES=${GSL_LIBRARIES} GSL_INCLUDE_DIRS=${GSL_INCLUDE_DIRS}")
find_package(LHAPDF REQUIRED)
message(STATUS "CASCADE: LHAPDF_VERSION=${LHAPDF_VERSION} LHAPDF_LIBRARIES=${LHAPDF_LIBRARIES} LHAPDF_INCLUDE_DIRS=${LHAPDF_INCLUDE_DIRS}")

######OPTIONAL PACKAGES#################################################
if (CASCADE_HEPMC2 AND CASCADE_HEPMC3)
  message(STATUS "CASCADE: Both options CASCADE_HEPMC2 and CASCADE_HEPMC3 were requsted. Only CASCADE_HEPMC3 will be used. To use HEPMC2 library set -DCASCADE_HEPMC3=OFF -DCASCADE_HEPMC2=ON in the command line")
  set(CASCADE_HEPMC2 OFF)
endif()
set ( conv_source ${PROJECT_SOURCE_DIR}/src/convhepmc-helpers.F  ${PROJECT_SOURCE_DIR}/src/Pythia6ToHepMC.cc ${PROJECT_SOURCE_DIR}/src/RivetInterface.cc)


if (CASCADE_HEPMC2)
  find_package(HepMC2 COMPONENTS FIO)
endif()
if (CASCADE_HEPMC2 AND HepMC2_FOUND)
  message(STATUS "CASCADE: HEPMC2_VERSION=${HEPMC2_VERSION} HEPMC2_LIBRARIES=${HEPMC2_LIBRARIES} HEPMC2_FIO_LIBRARIES=${HEPMC2_FIO_LIBRARIES} HEPMC2_INCLUDE_DIRS=${HEPMC2_INCLUDE_DIRS}")
endif()

if (CASCADE_HEPMC3)
  find_package(HepMC3)
endif()
if (CASCADE_HEPMC3 AND HepMC3_FOUND)
  message(STATUS "CASCADE: HEPMC3_VERSION=${HEPMC3_VERSION} HEPMC3_LIBRARIES=${HEPMC3_LIBRARIES}  HEPMC3_INCLUDE_DIR=${HEPMC3_INCLUDE_DIR}") 
endif()

if (NOT HepMC2_FOUND) 
  add_definitions(-DDUMMYPYTHIA6TOHEPMC2 )
endif()
if (NOT HepMC3_FOUND) 
  add_definitions(-DDUMMYPYTHIA6TOHEPMC3 -DPYTHIA6_USE_HEPMC2)
endif()


if (CASCADE_RIVET)
  if (CASCADE_HEPMC3 AND HepMC3_FOUND)
    find_package(Rivet COMPONENTS HEPMC3 )
  endif()
  if (CASCADE_HEPMC2 AND HepMC2_FOUND)
    find_package(Rivet COMPONENTS HEPMC2 )
  endif()
  message(STATUS "CASCADE: RIVET_FOUND=${RIVET_FOUND} RIVET_VERSION=${RIVET_VERSION} RIVET_LIBRARIES=${RIVET_LIBRARIES} RIVET_INCLUDE_DIRS=${RIVET_INCLUDE_DIRS} RIVET_DATA_PATH=${RIVET_DATA_PATH} RIVET_ANALYSIS_PATH=${RIVET_ANALYSIS_PATH} Rivet_HEPMC2_FOUND=${Rivet_HEPMC2_FOUND}  Rivet_HEPMC3_FOUND=${Rivet_HEPMC3_FOUND}")
endif()

if (NOT CASCADE_RIVET OR NOT RIVET_FOUND) 
  add_definitions(-DDUMMYRIVETINTERFACE)
endif()


if (CASCADE_TMDLIB)
  find_package(TMDLIB)
  message(STATUS "CASCADE: TMDLIB_VERSION=${TMDLIB_VERSION} TMDLIB_LIBRARIES=${TMDLIB_LIBRARIES} TMDLIB_INCLUDE_DIRS=${TMDLIB_INCLUDE_DIRS}")
endif()
if (CASCADE_TMDLIB AND TMDLIB_FOUND)
  set (tmdlib_source )
else()
  set (tmdlib_source ${PROJECT_SOURCE_DIR}/src/tmdlib-dummy.F)
endif()

if (CASCADE_PYTHIA8)
  find_package(Pythia8)
  message(STATUS "CASCADE: PYTHIA8_VERSION=${PYTHIA8_VERSION} PYTHIA8_LIBRARIES=${PYTHIA8_LIBRARIES} PYTHIA8_INCLUDE_DIRS=${PYTHIA8_INCLUDE_DIRS}")
endif()
if (CASCADE_PYTHIA8 AND Pythia8_FOUND)
  set(pythia8_source ${PROJECT_SOURCE_DIR}/src/pythia8frag.cc)
else()
  set(pythia8_source ${PROJECT_SOURCE_DIR}/src/pythia8frag-dummy.cc)
endif()

if (CASCADE_TMDLIB_EW)
  set(casc_source ${PROJECT_SOURCE_DIR}/src/casTMD.F )
else()
  set(casc_source ${PROJECT_SOURCE_DIR}/src/casTMDnoEW.F )
endif()

if (CASCADE_BUILD_DOCS)
  find_package(LATEX COMPONENTS PDFLATEX MAKEINDEX)
  message(STATUS "CASCADE: LATEX_FOUND=${LATEX_FOUND} PDFLATEX_COMPILER=${PDFLATEX_COMPILER} MAKEINDEX_COMPILER=${MAKEINDEX_COMPILER}")
endif()

#Create version files###################################################
if (CASCADE_CREATE_VERSION_FILES)
  file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/include/casvers.inc"   "        VERSQQ = \'${CASCADE_VERSION}\'\n")
  if (CASCADE_TMDLIB AND TMDLIB_FOUND)
    file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/include/casvers.inc" "        VERSTMDLIB = \'${TMDLIB_VERSION}\'\n")
  else()
    file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/include/casvers.inc" "        VERSTMDLIB = \'0.0.0\'\n")  
  endif()
  file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/include/casvers-tex.inc"   "        VERSQQ = \'${CASCADE_VERSION}\'\n")
endif()

#CASCADE3###############################################################
set(libcascade3_sources 
 ${PROJECT_SOURCE_DIR}/src/aalam.F       ${PROJECT_SOURCE_DIR}/src/caps.F       ${PROJECT_SOURCE_DIR}/src/cascha.F    ${PROJECT_SOURCE_DIR}/src/caupevnt.F  ${PROJECT_SOURCE_DIR}/src/dfun.F   
 ${PROJECT_SOURCE_DIR}/src/event.F       ${PROJECT_SOURCE_DIR}/src/mscatt.F     ${PROJECT_SOURCE_DIR}/src/steer.F     ${PROJECT_SOURCE_DIR}/src/xsecpsi.F   
 ${PROJECT_SOURCE_DIR}/src/alphas.F      ${PROJECT_SOURCE_DIR}/src/caps_ms.F    ${PROJECT_SOURCE_DIR}/src/cascps.F    ${PROJECT_SOURCE_DIR}/src/caupinit.F  ${PROJECT_SOURCE_DIR}/src/dgrv_nln.F 
 ${PROJECT_SOURCE_DIR}/src/fxn1.F        ${PROJECT_SOURCE_DIR}/src/multscatt.F  ${PROJECT_SOURCE_DIR}/src/pyr.F       ${PROJECT_SOURCE_DIR}/src/ccfmstfq.F   
 ${PROJECT_SOURCE_DIR}/src/bcasin.F      ${PROJECT_SOURCE_DIR}/src/casgen.F     ${PROJECT_SOURCE_DIR}/src/ccfmstfg.F  ${PROJECT_SOURCE_DIR}/src/dot1.F  
 ${PROJECT_SOURCE_DIR}/src/fxnb.F        ${PROJECT_SOURCE_DIR}/src/parti.F      ${PROJECT_SOURCE_DIR}/src/pytcha.F  
 ${PROJECT_SOURCE_DIR}/src/blueml.F      ${PROJECT_SOURCE_DIR}/src/caremh.F     ${PROJECT_SOURCE_DIR}/src/casini.F    ${PROJECT_SOURCE_DIR}/src/ccfmstfu.F  ${PROJECT_SOURCE_DIR}/src/dot.F  
 ${PROJECT_SOURCE_DIR}/src/ktstfu.F      ${PROJECT_SOURCE_DIR}/src/p_gamgam.F   ${PROJECT_SOURCE_DIR}/src/xglu.F 
 ${PROJECT_SOURCE_DIR}/src/caend.F       ${PROJECT_SOURCE_DIR}/src/caremn.F     ${PROJECT_SOURCE_DIR}/src/cutg.F     
 ${PROJECT_SOURCE_DIR}/src/mehiggs.F     ${PROJECT_SOURCE_DIR}/src/phase.F      ${PROJECT_SOURCE_DIR}/src/xgx.F 
 ${PROJECT_SOURCE_DIR}/src/caprikt.F     ${PROJECT_SOURCE_DIR}/src/casbran.F    ${PROJECT_SOURCE_DIR}/src/caspli.F    ${PROJECT_SOURCE_DIR}/src/dcasrn.F    ${PROJECT_SOURCE_DIR}/src/d_xgx.F 
 ${PROJECT_SOURCE_DIR}/src/meoffsh.F     ${PROJECT_SOURCE_DIR}/src/p_mscatt.F   ${PROJECT_SOURCE_DIR}/src/ranums.F    ${PROJECT_SOURCE_DIR}/src/xhiggs.F 
 ${PROJECT_SOURCE_DIR}/src/capsee.F      ${PROJECT_SOURCE_DIR}/src/cascade.F    ${PROJECT_SOURCE_DIR}/src/cauniglu.F  ${PROJECT_SOURCE_DIR}/src/dfridr.F    ${PROJECT_SOURCE_DIR}/src/elemnz.F 
 ${PROJECT_SOURCE_DIR}/src/minuts.F      ${PROJECT_SOURCE_DIR}/src/p_semih.F    ${PROJECT_SOURCE_DIR}/src/ryssha.F    ${PROJECT_SOURCE_DIR}/src/xsec1.F     ${PROJECT_SOURCE_DIR}/src/xsecqg.F  
 ${PROJECT_SOURCE_DIR}/src/meoffjpsi.F   ${PROJECT_SOURCE_DIR}/src/dhfint.F   
 ${PROJECT_SOURCE_DIR}/src/colorflow.F   ${PROJECT_SOURCE_DIR}/src/xvq.F        ${PROJECT_SOURCE_DIR}/src/mevq.F      ${PROJECT_SOURCE_DIR}/src/meqg.F      ${PROJECT_SOURCE_DIR}/src/pyadsh_cas.F 
 ${PROJECT_SOURCE_DIR}/src/mcint.F       ${PROJECT_SOURCE_DIR}/src/mcgen.F 
 ${PROJECT_SOURCE_DIR}/src/xvqq.F        ${PROJECT_SOURCE_DIR}/src/mevqq.F      ${PROJECT_SOURCE_DIR}/src/ktOrtAmVqNew.F 
 ${PROJECT_SOURCE_DIR}/src/xsecchi.F     ${PROJECT_SOURCE_DIR}/src/meoffchi.F   ${PROJECT_SOURCE_DIR}/src/mev.F       ${PROJECT_SOURCE_DIR}/src/xv1.F 
 ${PROJECT_SOURCE_DIR}/src/xsecjj.F      ${PROJECT_SOURCE_DIR}/src/meoffjj.F    ${PROJECT_SOURCE_DIR}/src/jj_ggf.F    ${PROJECT_SOURCE_DIR}/src/jj_me2.F 
 ${PROJECT_SOURCE_DIR}/src/input_steer.F ${PROJECT_SOURCE_DIR}/src/steer_new.F  ${PROJECT_SOURCE_DIR}/src/steer_pythia6.F 
 ${PROJECT_SOURCE_DIR}/src/ME2pythia.F   ${PROJECT_SOURCE_DIR}/src/ktclusdble.F ${PROJECT_SOURCE_DIR}/src/getjet.F    ${PROJECT_SOURCE_DIR}/src/mlmmatch.F   
 ${PROJECT_SOURCE_DIR}/src/cas_merging.F ${PROJECT_SOURCE_DIR}/src/alpsor.F 
 ${PROJECT_SOURCE_DIR}/src/cas_lheread.F ${PROJECT_SOURCE_DIR}/src/casbran_new.F 
 ${casc_source} 
 ${tmdlib_source}        
 ${PROJECT_SOURCE_DIR}/src/get_ips.cc ${PROJECT_SOURCE_DIR}/src/histo.cc    
 ${PROJECT_SOURCE_DIR}/src/rn_mixmax.cc    
 )
add_library(cascade3 SHARED ${libcascade3_sources})
target_include_directories(cascade3 PRIVATE ${PROJECT_SOURCE_DIR}/include)
install(TARGETS cascade3 DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT libs)
set_target_properties(cascade3 PROPERTIES LINKER_LANGUAGE Fortran)

##PYTHIA6 ##############################################################
set(cascadepythia_SOURCES   
 ${PROJECT_SOURCE_DIR}/pythia6428/fhhiggscorr.f ${PROJECT_SOURCE_DIR}/pythia6428/pyct5l.f ${PROJECT_SOURCE_DIR}/pythia6428/pygfxx.f ${PROJECT_SOURCE_DIR}/pythia6428/pylamf.f ${PROJECT_SOURCE_DIR}/pythia6428/pypole.f ${PROJECT_SOURCE_DIR}/pythia6428/pysfdc.f ${PROJECT_SOURCE_DIR}/pythia6428/pytime.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/fhsetflags.f ${PROJECT_SOURCE_DIR}/pythia6428/pyct5m.f ${PROJECT_SOURCE_DIR}/pythia6428/pyggam.f ${PROJECT_SOURCE_DIR}/pythia6428/pyldcm.f ${PROJECT_SOURCE_DIR}/pythia6428/pyprep.f ${PROJECT_SOURCE_DIR}/pythia6428/pysgex.f ${PROJECT_SOURCE_DIR}/pythia6428/pytune.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/fhsetpara.f ${PROJECT_SOURCE_DIR}/pythia6428/pycteq.f ${PROJECT_SOURCE_DIR}/pythia6428/pygive.f ${PROJECT_SOURCE_DIR}/pythia6428/pylhef.f ${PROJECT_SOURCE_DIR}/pythia6428/pyptdi.f ${PROJECT_SOURCE_DIR}/pythia6428/pysghf.f ${PROJECT_SOURCE_DIR}/pythia6428/pyuedc.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pycttr.f ${PROJECT_SOURCE_DIR}/pythia6428/pyglui.f ${PROJECT_SOURCE_DIR}/pythia6428/pylist.f ${PROJECT_SOURCE_DIR}/pythia6428/pyptfs.f ${PROJECT_SOURCE_DIR}/pythia6428/pysghg.f ${PROJECT_SOURCE_DIR}/pythia6428/pyupda.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/py1ent.f ${PROJECT_SOURCE_DIR}/pythia6428/pydcyk.f ${PROJECT_SOURCE_DIR}/pythia6428/pygram.f ${PROJECT_SOURCE_DIR}/pythia6428/pylogo.f ${PROJECT_SOURCE_DIR}/pythia6428/pyptis.f ${PROJECT_SOURCE_DIR}/pythia6428/pysgqc.f ${PROJECT_SOURCE_DIR}/pythia6428/pyupev.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/py2ent.f ${PROJECT_SOURCE_DIR}/pythia6428/pydecy.f ${PROJECT_SOURCE_DIR}/pythia6428/pygraw.f ${PROJECT_SOURCE_DIR}/pythia6428/pymael.f ${PROJECT_SOURCE_DIR}/pythia6428/pyptmi.f ${PROJECT_SOURCE_DIR}/pythia6428/pysgsu.f ${PROJECT_SOURCE_DIR}/pythia6428/pyupin.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/py2frm.f ${PROJECT_SOURCE_DIR}/pythia6428/pydiff.f ${PROJECT_SOURCE_DIR}/pythia6428/pygrvd.f ${PROJECT_SOURCE_DIR}/pythia6428/pymass.f ${PROJECT_SOURCE_DIR}/pythia6428/pyqqbh.f ${PROJECT_SOURCE_DIR}/pythia6428/pysgtc.f ${PROJECT_SOURCE_DIR}/pythia6428/pyupre.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/py3ent.f ${PROJECT_SOURCE_DIR}/pythia6428/pydisg.f ${PROJECT_SOURCE_DIR}/pythia6428/pygrvl.f ${PROJECT_SOURCE_DIR}/pythia6428/pymaxi.f ${PROJECT_SOURCE_DIR}/pythia6428/pyr.f ${PROJECT_SOURCE_DIR}/pythia6428/pysgwz.f ${PROJECT_SOURCE_DIR}/pythia6428/pyveto.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/py4ent.f ${PROJECT_SOURCE_DIR}/pythia6428/pydocu.f ${PROJECT_SOURCE_DIR}/pythia6428/pygrvm.f ${PROJECT_SOURCE_DIR}/pythia6428/pymemx.f ${PROJECT_SOURCE_DIR}/pythia6428/pyradk.f ${PROJECT_SOURCE_DIR}/pythia6428/pyshow.f ${PROJECT_SOURCE_DIR}/pythia6428/pywaux.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/py4frm.f ${PROJECT_SOURCE_DIR}/pythia6428/pydump.f ${PROJECT_SOURCE_DIR}/pythia6428/pygrvs.f ${PROJECT_SOURCE_DIR}/pythia6428/pymewt.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrand.f ${PROJECT_SOURCE_DIR}/pythia6428/pysigh.f ${PROJECT_SOURCE_DIR}/pythia6428/pywdkk.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/py4jet.f ${PROJECT_SOURCE_DIR}/pythia6428/pyedit.f ${PROJECT_SOURCE_DIR}/pythia6428/pygrvv.f ${PROJECT_SOURCE_DIR}/pythia6428/pymign.f ${PROJECT_SOURCE_DIR}/pythia6428/pyreco.f ${PROJECT_SOURCE_DIR}/pythia6428/pysimp.f ${PROJECT_SOURCE_DIR}/pythia6428/pywidt.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/py4jts.f ${PROJECT_SOURCE_DIR}/pythia6428/pyeevt.f ${PROJECT_SOURCE_DIR}/pythia6428/pygrvw.f ${PROJECT_SOURCE_DIR}/pythia6428/pymihg.f ${PROJECT_SOURCE_DIR}/pythia6428/pyremn.f ${PROJECT_SOURCE_DIR}/pythia6428/pyslha.f ${PROJECT_SOURCE_DIR}/pythia6428/pywidx.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/py4jtw.f ${PROJECT_SOURCE_DIR}/pythia6428/pyeicg.f ${PROJECT_SOURCE_DIR}/pythia6428/pygvmd.f ${PROJECT_SOURCE_DIR}/pythia6428/pymihk.f ${PROJECT_SOURCE_DIR}/pythia6428/pyresd.f ${PROJECT_SOURCE_DIR}/pythia6428/pyspen.f ${PROJECT_SOURCE_DIR}/pythia6428/pyx2xg.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/py6frm.f ${PROJECT_SOURCE_DIR}/pythia6428/pyeig4.f ${PROJECT_SOURCE_DIR}/pythia6428/pyh2xx.f ${PROJECT_SOURCE_DIR}/pythia6428/pymirm.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrget.f ${PROJECT_SOURCE_DIR}/pythia6428/pysphe.f ${PROJECT_SOURCE_DIR}/pythia6428/pyx2xh.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pyadsh.f ${PROJECT_SOURCE_DIR}/pythia6428/pyerrm.f ${PROJECT_SOURCE_DIR}/pythia6428/pyhepc.f ${PROJECT_SOURCE_DIR}/pythia6428/pymrun.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrghm.f ${PROJECT_SOURCE_DIR}/pythia6428/pyspli.f ${PROJECT_SOURCE_DIR}/pythia6428/pyx3jt.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pyalem.f ${PROJECT_SOURCE_DIR}/pythia6428/pyevnt.f ${PROJECT_SOURCE_DIR}/pythia6428/pyhext.f ${PROJECT_SOURCE_DIR}/pythia6428/pymsin.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrnm3.f ${PROJECT_SOURCE_DIR}/pythia6428/pysspa.f ${PROJECT_SOURCE_DIR}/pythia6428/pyx4jt.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pyalps.f ${PROJECT_SOURCE_DIR}/pythia6428/pyevnw.f ${PROJECT_SOURCE_DIR}/pythia6428/pyhfth.f ${PROJECT_SOURCE_DIR}/pythia6428/pymult.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrnmq.f ${PROJECT_SOURCE_DIR}/pythia6428/pystat.f ${PROJECT_SOURCE_DIR}/pythia6428/pyxdif.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pyangl.f ${PROJECT_SOURCE_DIR}/pythia6428/pyevol.f ${PROJECT_SOURCE_DIR}/pythia6428/pyhggm.f ${PROJECT_SOURCE_DIR}/pythia6428/pyname.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrobo.f ${PROJECT_SOURCE_DIR}/pythia6428/pystbh.f ${PROJECT_SOURCE_DIR}/pythia6428/pyxdin.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pyapps.f ${PROJECT_SOURCE_DIR}/pythia6428/pyevwt.f ${PROJECT_SOURCE_DIR}/pythia6428/pyhist.f ${PROJECT_SOURCE_DIR}/pythia6428/pynjdc.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrset.f ${PROJECT_SOURCE_DIR}/pythia6428/pystop.f ${PROJECT_SOURCE_DIR}/pythia6428/pyxjet.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pybesq.f ${PROJECT_SOURCE_DIR}/pythia6428/pyexec.f ${PROJECT_SOURCE_DIR}/pythia6428/pyi3au.f ${PROJECT_SOURCE_DIR}/pythia6428/pynmes.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrvch.f ${PROJECT_SOURCE_DIR}/pythia6428/pystrf.f ${PROJECT_SOURCE_DIR}/pythia6428/pyxkfl.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pybksb.f ${PROJECT_SOURCE_DIR}/pythia6428/pyfact.f ${PROJECT_SOURCE_DIR}/pythia6428/pyinbm.f ${PROJECT_SOURCE_DIR}/pythia6428/pynull.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrvg1.f ${PROJECT_SOURCE_DIR}/pythia6428/pysubh.f ${PROJECT_SOURCE_DIR}/pythia6428/pyxtee.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pyboei.f ${PROJECT_SOURCE_DIR}/pythia6428/pyfcmp.f ${PROJECT_SOURCE_DIR}/pythia6428/pyindf.f ${PROJECT_SOURCE_DIR}/pythia6428/pyofsh.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrvg2.f ${PROJECT_SOURCE_DIR}/pythia6428/pysugi.f ${PROJECT_SOURCE_DIR}/pythia6428/pyxtot.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pybook.f ${PROJECT_SOURCE_DIR}/pythia6428/pyfeyn.f ${PROJECT_SOURCE_DIR}/pythia6428/pyinit.f ${PROJECT_SOURCE_DIR}/pythia6428/pyonia.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrvg3.f ${PROJECT_SOURCE_DIR}/pythia6428/pytabu.f ${PROJECT_SOURCE_DIR}/pythia6428/pyxued.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pycba2.f ${PROJECT_SOURCE_DIR}/pythia6428/pyfill.f ${PROJECT_SOURCE_DIR}/pythia6428/pyinki.f ${PROJECT_SOURCE_DIR}/pythia6428/pyonof.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrvg4.f ${PROJECT_SOURCE_DIR}/pythia6428/pytaud.f ${PROJECT_SOURCE_DIR}/pythia6428/pyxxga.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pycbal.f ${PROJECT_SOURCE_DIR}/pythia6428/pyfint.f ${PROJECT_SOURCE_DIR}/pythia6428/pyinom.f ${PROJECT_SOURCE_DIR}/pythia6428/pyoper.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrvgl.f ${PROJECT_SOURCE_DIR}/pythia6428/pytbbc.f ${PROJECT_SOURCE_DIR}/pythia6428/pyxxz6.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pycdiv.f ${PROJECT_SOURCE_DIR}/pythia6428/pyfisb.f ${PROJECT_SOURCE_DIR}/pythia6428/pyinpr.f ${PROJECT_SOURCE_DIR}/pythia6428/pyp.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrvgw.f ${PROJECT_SOURCE_DIR}/pythia6428/pytbbn.f ${PROJECT_SOURCE_DIR}/pythia6428/pyzdis.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pycell.f ${PROJECT_SOURCE_DIR}/pythia6428/pyfowo.f ${PROJECT_SOURCE_DIR}/pythia6428/pyinre.f ${PROJECT_SOURCE_DIR}/pythia6428/pypcmp.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrvi1.f ${PROJECT_SOURCE_DIR}/pythia6428/pytbdy.f ${PROJECT_SOURCE_DIR}/pythia6428/ssmssm.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pychge.f ${PROJECT_SOURCE_DIR}/pythia6428/pyfram.f ${PROJECT_SOURCE_DIR}/pythia6428/pyjmas.f ${PROJECT_SOURCE_DIR}/pythia6428/pypdel.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrvi2.f ${PROJECT_SOURCE_DIR}/pythia6428/pytbhb.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pycjdc.f ${PROJECT_SOURCE_DIR}/pythia6428/pyfscr.f ${PROJECT_SOURCE_DIR}/pythia6428/pyjoin.f ${PROJECT_SOURCE_DIR}/pythia6428/pypdfl.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrvi3.f ${PROJECT_SOURCE_DIR}/pythia6428/pytbhg.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pyckbd.f ${PROJECT_SOURCE_DIR}/pythia6428/pygaga.f ${PROJECT_SOURCE_DIR}/pythia6428/pyjurf.f ${PROJECT_SOURCE_DIR}/pythia6428/pypdfu.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrvne.f ${PROJECT_SOURCE_DIR}/pythia6428/pytbhq.f ${PROJECT_SOURCE_DIR}/pythia6428/sugra.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pyclus.f ${PROJECT_SOURCE_DIR}/pythia6428/pygamm.f ${PROJECT_SOURCE_DIR}/pythia6428/pyk.f ${PROJECT_SOURCE_DIR}/pythia6428/pypdga.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrvr.f ${PROJECT_SOURCE_DIR}/pythia6428/pytbhs.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pycmq2.f ${PROJECT_SOURCE_DIR}/pythia6428/pygano.f ${PROJECT_SOURCE_DIR}/pythia6428/pykcut.f ${PROJECT_SOURCE_DIR}/pythia6428/pypdpi.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrvs.f ${PROJECT_SOURCE_DIR}/pythia6428/pytecm.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pycmqr.f ${PROJECT_SOURCE_DIR}/pythia6428/pygau2.f ${PROJECT_SOURCE_DIR}/pythia6428/pykfdi.f ${PROJECT_SOURCE_DIR}/pythia6428/pypdpo.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrvsb.f ${PROJECT_SOURCE_DIR}/pythia6428/pytest.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pycomp.f ${PROJECT_SOURCE_DIR}/pythia6428/pygaus.f ${PROJECT_SOURCE_DIR}/pythia6428/pykfin.f ${PROJECT_SOURCE_DIR}/pythia6428/pypdpr.f ${PROJECT_SOURCE_DIR}/pythia6428/pyrvsf.f ${PROJECT_SOURCE_DIR}/pythia6428/pythag.f ${PROJECT_SOURCE_DIR}/pythia6428/visaje.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pycrth.f ${PROJECT_SOURCE_DIR}/pythia6428/pygbeh.f ${PROJECT_SOURCE_DIR}/pythia6428/pyklim.f ${PROJECT_SOURCE_DIR}/pythia6428/pypile.f ${PROJECT_SOURCE_DIR}/pythia6428/pysave.f ${PROJECT_SOURCE_DIR}/pythia6428/pythrg.f 
 ${PROJECT_SOURCE_DIR}/pythia6428/pycsrt.f ${PROJECT_SOURCE_DIR}/pythia6428/pygdir.f ${PROJECT_SOURCE_DIR}/pythia6428/pykmap.f ${PROJECT_SOURCE_DIR}/pythia6428/pyplot.f ${PROJECT_SOURCE_DIR}/pythia6428/pyscat.f ${PROJECT_SOURCE_DIR}/pythia6428/pythru.f
)
add_library(cascadepythia SHARED ${cascadepythia_SOURCES})
target_include_directories(cascadepythia PRIVATE ${PROJECT_SOURCE_DIR}/include)
install(TARGETS cascadepythia DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT libs)
set_target_properties(cascadepythia PROPERTIES LINKER_LANGUAGE Fortran)

##BASES ################################################################
set(libcascadebases_la_SOURCES  
 ${PROJECT_SOURCE_DIR}/bases51/bases.F ${PROJECT_SOURCE_DIR}/bases51/bschck.F ${PROJECT_SOURCE_DIR}/bases51/bsgetw.F ${PROJECT_SOURCE_DIR}/bases51/bsintg.F ${PROJECT_SOURCE_DIR}/bases51/bsputw.F 
 ${PROJECT_SOURCE_DIR}/bases51/bswrit.F ${PROJECT_SOURCE_DIR}/bases51/drnset.F ${PROJECT_SOURCE_DIR}/bases51/shplot.F ${PROJECT_SOURCE_DIR}/bases51/sphist.F ${PROJECT_SOURCE_DIR}/bases51/xhfill.F
 ${PROJECT_SOURCE_DIR}/bases51/xhscle.F ${PROJECT_SOURCE_DIR}/bases51/bhinit.F ${PROJECT_SOURCE_DIR}/bases51/bsdate.F ${PROJECT_SOURCE_DIR}/bases51/bsgrid.F ${PROJECT_SOURCE_DIR}/bases51/bslist.F 
 ${PROJECT_SOURCE_DIR}/bases51/bsread.F ${PROJECT_SOURCE_DIR}/bases51/dhfill.F  ${PROJECT_SOURCE_DIR}/bases51/shrset.F ${PROJECT_SOURCE_DIR}/bases51/spinfo.F 
 ${PROJECT_SOURCE_DIR}/bases51/xhinit.F ${PROJECT_SOURCE_DIR}/bases51/bhplot.F ${PROJECT_SOURCE_DIR}/bases51/bsdims.F ${PROJECT_SOURCE_DIR}/bases51/bsordr.F 
 ${PROJECT_SOURCE_DIR}/bases51/bstcnv.F ${PROJECT_SOURCE_DIR}/bases51/dhinit.F ${PROJECT_SOURCE_DIR}/bases51/shcler.F ${PROJECT_SOURCE_DIR}/bases51/shupdt.F ${PROJECT_SOURCE_DIR}/bases51/sprgen.F
 ${PROJECT_SOURCE_DIR}/bases51/xhordr.F ${PROJECT_SOURCE_DIR}/bases51/bhrset.F ${PROJECT_SOURCE_DIR}/bases51/bsetgu.F ${PROJECT_SOURCE_DIR}/bases51/bsinfo.F ${PROJECT_SOURCE_DIR}/bases51/bsparm.F
 ${PROJECT_SOURCE_DIR}/bases51/bstime.F ${PROJECT_SOURCE_DIR}/bases51/dhplot.F ${PROJECT_SOURCE_DIR}/bases51/shfill.F ${PROJECT_SOURCE_DIR}/bases51/spchck.F ${PROJECT_SOURCE_DIR}/bases51/spring.F
 ${PROJECT_SOURCE_DIR}/bases51/xhplot.F ${PROJECT_SOURCE_DIR}/bases51/bhsave.F ${PROJECT_SOURCE_DIR}/bases51/bsetgv.F ${PROJECT_SOURCE_DIR}/bases51/bsinit.F ${PROJECT_SOURCE_DIR}/bases51/bsprnt.F
 ${PROJECT_SOURCE_DIR}/bases51/bsutim.F ${PROJECT_SOURCE_DIR}/bases51/shinit.F ${PROJECT_SOURCE_DIR}/bases51/xhchck.F
 ${PROJECT_SOURCE_DIR}/bases51/xhrnge.F
 )
add_library(cascadebases SHARED ${libcascadebases_la_SOURCES})
target_include_directories(cascadebases PRIVATE ${PROJECT_SOURCE_DIR}/include)
install(TARGETS cascadebases DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT libs)
set_target_properties(cascadebases PROPERTIES LINKER_LANGUAGE Fortran)

##MYCERN ###############################################################
set(cascademycern_la_SOURCES  ${PROJECT_SOURCE_DIR}/misc/abend.F  ${PROJECT_SOURCE_DIR}/misc/besi0.F  ${PROJECT_SOURCE_DIR}/misc/expint.F  ${PROJECT_SOURCE_DIR}/misc/ranlux.F  ${PROJECT_SOURCE_DIR}/misc/rnpssn.F  
                         ${PROJECT_SOURCE_DIR}/misc/ucopy.F  ${PROJECT_SOURCE_DIR}/misc/vzero.F ${PROJECT_SOURCE_DIR}/misc/dgauss.F ${PROJECT_SOURCE_DIR}/misc/rnormx.F 
                         ${PROJECT_SOURCE_DIR}/misc/datime.F ${PROJECT_SOURCE_DIR}/misc/timex.F ${PROJECT_SOURCE_DIR}/misc/timest.F  ${PROJECT_SOURCE_DIR}/misc/dminfc.F 
                         )
add_library(cascademycern SHARED ${cascademycern_la_SOURCES})
target_include_directories(cascademycern PRIVATE ${PROJECT_SOURCE_DIR}/include)
install(TARGETS cascademycern DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT libs)
set_target_properties(cascademycern PROPERTIES LINKER_LANGUAGE Fortran)

##CASCADE BIN ##########################################################
set( cascade_SOURCES  ${PROJECT_SOURCE_DIR}/src/casmain.F  ${PROJECT_SOURCE_DIR}/src/drn.F  ${PROJECT_SOURCE_DIR}/src/upevnt.F ${PROJECT_SOURCE_DIR}/src/upinit.F ${PROJECT_SOURCE_DIR}/src/pydata.F
                      ${PROJECT_SOURCE_DIR}/src/fjfort.cc ${PROJECT_SOURCE_DIR}/src/fjcore.cc ${PROJECT_SOURCE_DIR}/src/gzstuff.cc 
                      ${conv_source}
                      ${pythia8_source}   
)

add_executable(cascade ${cascade_SOURCES})
target_include_directories(cascade PRIVATE ${PROJECT_SOURCE_DIR}/include  ${ZLIB_INCLUDE_DIRS} ${GSL_INCLUDE_DIRS} ${LHAPDF_INCLUDE_DIR})
target_link_libraries(cascade PRIVATE cascade3 cascadepythia cascadebases cascademycern ${ZLIB_LIBRARIES} ${GSL_LIBRARIES} ${LHAPDF_LIBRARIES})
set_target_properties(cascade PROPERTIES LINKER_LANGUAGE Fortran)

if (CASCADE_HEPMC2 AND HepMC2_FOUND)
  target_include_directories(cascade PRIVATE  ${HEPMC2_INCLUDE_DIRS})
  target_link_libraries(cascade PRIVATE ${HEPMC2_LIBRARIES} ${HEPMC2_FIO_LIBRARIES})
endif()

if (CASCADE_HEPMC3 AND HepMC3_FOUND)
  target_include_directories(cascade PRIVATE  ${HEPMC3_INCLUDE_DIR})
  target_link_libraries(cascade PRIVATE ${HEPMC3_LIBRARIES})
  target_compile_features(cascade PUBLIC cxx_std_11)
endif()

if (CASCADE_RIVET AND RIVET_FOUND)
  target_include_directories(cascade PRIVATE  ${RIVET_INCLUDE_DIR}) 
  target_link_libraries(cascade PRIVATE ${RIVET_LIBRARIES})
  target_compile_features(cascade PUBLIC cxx_std_14)
endif()

if (CASCADE_PYTHIA8 AND Pythia8_FOUND)
  target_include_directories(cascade PRIVATE  ${PYTHIA8_INCLUDE_DIR}) 
  target_link_libraries(cascade PRIVATE ${PYTHIA8_LIBRARIES})
endif()

if (CASCADE_TMDLIB AND TMDLIB_FOUND)
  target_include_directories(cascade PRIVATE  ${TMDLIB_INCLUDE_DIR}) 
  target_link_libraries(cascade PRIVATE ${TMDLIB_LIBRARIES})
endif()
install(TARGETS cascade DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT binary)

## MANUAL  #############################################################
if (CASCADE_BUILD_DOCS AND LATEX_FOUND AND LATEX_PDFLATEX_FOUND AND LATEX_MAKEINDEX_FOUND)
  set( manual_sources 
       ${PROJECT_SOURCE_DIR}/manual/cascade3.tex 
       ${PROJECT_SOURCE_DIR}/manual/cascade3.bbl 
       ${PROJECT_SOURCE_DIR}/manual/mystyle-new.sty 
       ${PROJECT_SOURCE_DIR}/manual/ladder-ccfm.eps 
       ${PROJECT_SOURCE_DIR}/manual/ref.bib 
       ${PROJECT_SOURCE_DIR}/manual/mcite.sty 
       ${PROJECT_SOURCE_DIR}/manual/heralhc.bst 
       ${PROJECT_SOURCE_DIR}/manual/parton-shower-evolution-crop.pdf 
       ${PROJECT_SOURCE_DIR}/manual/splitting-process-crop.pdf
       ${PROJECT_SOURCE_DIR}/manual/ladder-ccfm.pdf 
       ${PROJECT_SOURCE_DIR}/manual/NLO-DiMuon_mass-13TeV.pdf 
       ${PROJECT_SOURCE_DIR}/manual/NLO-DiMuon_pT-13TeV.pdf 
       ${PROJECT_SOURCE_DIR}/manual/NLO-DiMuon_y-13TeV.pdf 
       ${PROJECT_SOURCE_DIR}/manual/meint-pp-13TeV-bbar-ttbar.pdf 
       ${PROJECT_SOURCE_DIR}/manual/meint-pp-13TeV-bbar.pdf
     )
     file(COPY ${manual_sources} DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/manual)
     ADD_CUSTOM_TARGET( latexcompileall
                       COMMAND ${PDFLATEX_COMPILER} cascade3
                       COMMAND ${PDFLATEX_COMPILER} cascade3
                       COMMAND ${MAKEINDEX_COMPILER} cascade3
                       COMMAND ${PDFLATEX_COMPILER} cascade3
                       COMMENT "Creating manual "
                       WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/manual
                       DEPENDS ${PROJECT_SOURCE_DIR}/manual/cascade3.tex
                      )
     ADD_CUSTOM_TARGET(all-formats ALL) # Entry point of execution.
     ADD_DEPENDENCIES( all-formats latexcompileall)
     install(FILES ${CMAKE_CURRENT_BINARY_DIR}/manual/cascade3.pdf DESTINATION  ${CMAKE_INSTALL_DOCDIR} COMPONENT doc)
endif()
### DATA ###############################################################
set(lhe 
          ${PROJECT_SOURCE_DIR}/data/steering-DY-MCatNLO.txt 
          ${PROJECT_SOURCE_DIR}/data/steering-DY-PH.txt 
          ${PROJECT_SOURCE_DIR}/data/steering-KaTie.txt
          ${PROJECT_SOURCE_DIR}/data/steering-KaTie-DIS.txt
          ${PROJECT_SOURCE_DIR}/data/steering.txt 

          ${PROJECT_SOURCE_DIR}/data/POWHEG-example.lhe.gz 
          ${PROJECT_SOURCE_DIR}/data/MCatNLO-example.lhe.gz 
          ${PROJECT_SOURCE_DIR}/data/Katie-example.lhe.gz 
          ${PROJECT_SOURCE_DIR}/data/steering-DY-merging.txt 
          ${PROJECT_SOURCE_DIR}/data/steering-DY-MCatNLO-pA.txt 
          ${PROJECT_SOURCE_DIR}/data/steering-fNLO.txt 
          ${PROJECT_SOURCE_DIR}/data/DY-13TeV-fNLO-TMDset2.lhe.gz 
          ${PROJECT_SOURCE_DIR}/data/MG5_Z+0+1+2_MLM_merging.lhe.gz
          ${PROJECT_SOURCE_DIR}/data/steering-P8.cmnd
    )


set(pp_old-steering ${PROJECT_SOURCE_DIR}/data/steer_pp-bottom ${PROJECT_SOURCE_DIR}/data/steer_pp-top ${PROJECT_SOURCE_DIR}/data/steer_pp-higgs 
         ${PROJECT_SOURCE_DIR}/data/steer_pp ${PROJECT_SOURCE_DIR}/data/steer_pp-jpsi ${PROJECT_SOURCE_DIR}/data/steer_pp-chi 
         ${PROJECT_SOURCE_DIR}/data/steer_pp-ZQQ ${PROJECT_SOURCE_DIR}/data/steer_pp-Zq ${PROJECT_SOURCE_DIR}/data/steer_pp-Wq 
         ${PROJECT_SOURCE_DIR}/data/steer_pp-jpsijpsi ${PROJECT_SOURCE_DIR}/data/pythia64-update-particle.dat 
    )

set(pp ${PROJECT_SOURCE_DIR}/data/steering_pp-Wq.txt       
         ${PROJECT_SOURCE_DIR}/data/steering_pp-bottom.txt   ${PROJECT_SOURCE_DIR}/data/steering_pp-jpsi.txt     
         ${PROJECT_SOURCE_DIR}/data/steering_pp.txt          ${PROJECT_SOURCE_DIR}/data/steering_pp-ZQQ.txt      
         ${PROJECT_SOURCE_DIR}/data/steering_pp-chi.txt      ${PROJECT_SOURCE_DIR}/data/steering_pp-jpsijpsi.txt
         ${PROJECT_SOURCE_DIR}/data/steering_pp-Zq.txt       ${PROJECT_SOURCE_DIR}/data/steering_pp-higgs.txt    
         ${PROJECT_SOURCE_DIR}/data/steering_pp-top.txt      ${PROJECT_SOURCE_DIR}/data/pythia64-update-particle.dat
         )


set(hera_old-steering ${PROJECT_SOURCE_DIR}/data/steer_ep 
         ${PROJECT_SOURCE_DIR}/data/steer_ep-charm 
         ${PROJECT_SOURCE_DIR}/data/steer_ep-jpsi 
         ${PROJECT_SOURCE_DIR}/data/pythia64-update-particle.dat 
         )
set(ep ${PROJECT_SOURCE_DIR}/data/steering_ep-charm.txt 
         ${PROJECT_SOURCE_DIR}/data/steering_ep-jpsi.txt  
         ${PROJECT_SOURCE_DIR}/data/steering_ep.txt 
         ${PROJECT_SOURCE_DIR}/data/pythia64-update-particle.dat 
         )

set(updf   ${PROJECT_SOURCE_DIR}/data/ccfm-final-setA1.dat   ${PROJECT_SOURCE_DIR}/data/ccfm-ipgg0ns-1-0-all-lhc.dat  
             ${PROJECT_SOURCE_DIR}/data/ccfm-final-setB0-.dat  ${PROJECT_SOURCE_DIR}/data/ccfm-ipgg1ns1-0-all-lhc.dat   
             ${PROJECT_SOURCE_DIR}/data/ccfm.dat ${PROJECT_SOURCE_DIR}/data/ccfm-final-setB0.dat 
             ${PROJECT_SOURCE_DIR}/data/ccfm-ipgg2ns2-0-all-lhc.dat 
             ${PROJECT_SOURCE_DIR}/data/ccfm-final-setA0-.dat  ${PROJECT_SOURCE_DIR}/data/ccfm-final-setB0+.dat  
             ${PROJECT_SOURCE_DIR}/data/kmr.dat 
             ${PROJECT_SOURCE_DIR}/data/ccfm-final-setA0.dat   ${PROJECT_SOURCE_DIR}/data/ccfm-final-setB1.dat   
             ${PROJECT_SOURCE_DIR}/data/kms.dat  
             ${PROJECT_SOURCE_DIR}/data/ccfm-final-setA0+.dat  ${PROJECT_SOURCE_DIR}/data/ccfm-gam.dat 
             ${PROJECT_SOURCE_DIR}/data/ccfm-setC.dat 
             ${PROJECT_SOURCE_DIR}/data/ccfm-quark.dat 
             ${PROJECT_SOURCE_DIR}/data/JH-2013-set1-Q0+.dat ${PROJECT_SOURCE_DIR}/data/JH-2013-set1-Q0-.dat 
             ${PROJECT_SOURCE_DIR}/data/JH-2013-set1-muf+.dat 
             ${PROJECT_SOURCE_DIR}/data/JH-2013-set1-muf-.dat ${PROJECT_SOURCE_DIR}/data/JH-2013-set1-mur+.dat 
             ${PROJECT_SOURCE_DIR}/data/JH-2013-set1-mur-.dat 
             ${PROJECT_SOURCE_DIR}/data/JH-2013-set1-par1+.dat ${PROJECT_SOURCE_DIR}/data/JH-2013-set1-par1-.dat 
             ${PROJECT_SOURCE_DIR}/data/JH-2013-set1-par2+.dat 
             ${PROJECT_SOURCE_DIR}/data/JH-2013-set1-par2-.dat ${PROJECT_SOURCE_DIR}/data/JH-2013-set1-par3+.dat 
             ${PROJECT_SOURCE_DIR}/data/JH-2013-set1-par3-.dat 
             ${PROJECT_SOURCE_DIR}/data/JH-2013-set1.dat 
             ${PROJECT_SOURCE_DIR}/data/JH-2013-set2-Q0+.dat ${PROJECT_SOURCE_DIR}/data/JH-2013-set2-Q0-.dat 
             ${PROJECT_SOURCE_DIR}/data/JH-2013-set2-muf+.dat 
             ${PROJECT_SOURCE_DIR}/data/JH-2013-set2-muf-.dat ${PROJECT_SOURCE_DIR}/data/JH-2013-set2-mur+.dat 
             ${PROJECT_SOURCE_DIR}/data/JH-2013-set2-mur-.dat 
             ${PROJECT_SOURCE_DIR}/data/JH-2013-set2-par1+.dat ${PROJECT_SOURCE_DIR}/data/JH-2013-set2-par1-.dat 
             ${PROJECT_SOURCE_DIR}/data/JH-2013-set2-par2+.dat 
             ${PROJECT_SOURCE_DIR}/data/JH-2013-set2-par2-.dat ${PROJECT_SOURCE_DIR}/data/JH-2013-set2-par3+.dat 
             ${PROJECT_SOURCE_DIR}/data/JH-2013-set2-par3-.dat 
             ${PROJECT_SOURCE_DIR}/data/JH-2013-set2.dat
)
set(cascadeincludes 
${PROJECT_SOURCE_DIR}/include/caseffic.inc
${PROJECT_SOURCE_DIR}/include/casevent.inc
${PROJECT_SOURCE_DIR}/include/casfNLO.inc
${PROJECT_SOURCE_DIR}/include/casgki.inc
${PROJECT_SOURCE_DIR}/include/casglu.inc
${PROJECT_SOURCE_DIR}/include/casherup.inc
${PROJECT_SOURCE_DIR}/include/casiglu.inc
${PROJECT_SOURCE_DIR}/include/caslhe.inc
${PROJECT_SOURCE_DIR}/include/casluco.inc
${PROJECT_SOURCE_DIR}/include/casludat1.inc
${PROJECT_SOURCE_DIR}/include/casludat2.inc
${PROJECT_SOURCE_DIR}/include/casludat3.inc
${PROJECT_SOURCE_DIR}/include/caslujets.inc
${PROJECT_SOURCE_DIR}/include/casluparm.inc
${PROJECT_SOURCE_DIR}/include/casMEMAIN.inc
${PROJECT_SOURCE_DIR}/include/casMERGING.inc
${PROJECT_SOURCE_DIR}/include/casMLM.inc
${PROJECT_SOURCE_DIR}/include/casmult.inc
${PROJECT_SOURCE_DIR}/include/caspara1.inc
${PROJECT_SOURCE_DIR}/include/caspara5.inc
${PROJECT_SOURCE_DIR}/include/caspara.inc
${PROJECT_SOURCE_DIR}/include/casparam.inc
${PROJECT_SOURCE_DIR}/include/casparas.inc
${PROJECT_SOURCE_DIR}/include/caspar.inc
${PROJECT_SOURCE_DIR}/include/caspart.inc
${PROJECT_SOURCE_DIR}/include/casprkt.inc
${PROJECT_SOURCE_DIR}/include/caspsipol.inc
${PROJECT_SOURCE_DIR}/include/casscal.inc
${PROJECT_SOURCE_DIR}/include/casshwr.inc
${PROJECT_SOURCE_DIR}/include/casvalues.inc
${PROJECT_SOURCE_DIR}/include/casvers.inc
${PROJECT_SOURCE_DIR}/include/dateqq.inc
${PROJECT_SOURCE_DIR}/include/fjcore.hh
${PROJECT_SOURCE_DIR}/include/HepMCSteering.inc
${PROJECT_SOURCE_DIR}/include/Pythia6ToHepMC2.inc
${PROJECT_SOURCE_DIR}/include/Pythia6ToHepMC3.inc
${PROJECT_SOURCE_DIR}/include/Pythia6ToHepMC.inc
${PROJECT_SOURCE_DIR}/include/PythiaHelper.h
${PROJECT_SOURCE_DIR}/include/qcdnum.inc
${PROJECT_SOURCE_DIR}/include/RivetInterface.inc
${PROJECT_SOURCE_DIR}/include/timeqq.inc
${PROJECT_SOURCE_DIR}/include/mixmax.hpp
${PROJECT_SOURCE_DIR}/include/mixmax_skip_N17.c
${PROJECT_SOURCE_DIR}/include/mixmax_skip_N240.c
)
install(FILES  ${cascadeincludes} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/cascade/ COMPONENT devel) 

install(FILES ${pp_old-steering} DESTINATION  ${CMAKE_INSTALL_DATADIR}/cascade/PP_old-steering COMPONENT doc)
install(FILES ${pp} DESTINATION  ${CMAKE_INSTALL_DATADIR}/cascade/PP COMPONENT doc)
install(FILES ${lhe} DESTINATION  ${CMAKE_INSTALL_DATADIR}/cascade/LHE COMPONENT doc)
install(FILES ${hera_old-steering} DESTINATION  ${CMAKE_INSTALL_DATADIR}/cascade/HERA_old-steering COMPONENT doc)
install(FILES ${ep} DESTINATION  ${CMAKE_INSTALL_DATADIR}/cascade/EP COMPONENT doc)
install(FILES ${updf} DESTINATION  ${CMAKE_INSTALL_DATADIR}/cascade/UPDF COMPONENT doc)


########################################################################
macro(CascadeTestTargetOld nam tmd card)
    file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/UPDF/)
    file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/foo${nam})
    file(COPY ${tmd} DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/UPDF/)
    file(COPY ${tmd} DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/UPDF/)
    add_test(NAME test${nam} COMMAND sh -c "${PROJECT_BINARY_DIR}/cascade < ${card}"  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/foo${nam})
    SET_TESTS_PROPERTIES( test${nam} PROPERTIES ENVIRONMENT "PATH=${PROJECT_BINARY_DIR}/:$ENV{PATH};LD_LIBRARY_PATH=${PROJECT_BINARY_DIR}:$ENV{LD_LIBRARY_PATH};DYLD_LIBRARY_PATH=${PROJECT_BINARY_DIR}:$ENV{DYLD_LIBRARY_PATH}")
endmacro(CascadeTestTargetOld)
macro(CascadeTestTarget nam tmd input card)
    add_custom_target(test${nam}${tmd} ALL COMMAND TMDlib-getdata ${tmd})
    file(COPY ${input} DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
    add_test(NAME test${nam} COMMAND sh -c "${PROJECT_BINARY_DIR}/cascade < ${card}"  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
    SET_TESTS_PROPERTIES( test${nam} PROPERTIES ENVIRONMENT "PATH=${PROJECT_BINARY_DIR}/:$ENV{PATH};LD_LIBRARY_PATH=${PROJECT_BINARY_DIR}:$ENV{LD_LIBRARY_PATH};DYLD_LIBRARY_PATH=${PROJECT_BINARY_DIR}:$ENV{DYLD_LIBRARY_PATH}")
endmacro(CascadeTestTarget)
macro(CascadeTestTargetStandalone nam tmd  card)
    add_custom_target(test${nam}${tmd} ALL COMMAND TMDlib-getdata ${tmd})
    add_test(NAME test${nam} COMMAND sh -c "${PROJECT_BINARY_DIR}/cascade < ${card}"  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
    SET_TESTS_PROPERTIES( test${nam} PROPERTIES ENVIRONMENT "PATH=${PROJECT_BINARY_DIR}/:$ENV{PATH};LD_LIBRARY_PATH=${PROJECT_BINARY_DIR}:$ENV{LD_LIBRARY_PATH};DYLD_LIBRARY_PATH=${PROJECT_BINARY_DIR}:$ENV{DYLD_LIBRARY_PATH}")
endmacro(CascadeTestTargetStandalone)
if(CASCADE_ENABLE_TESTS)
  ENABLE_TESTING()
  CascadeTestTargetOld(ep    ${PROJECT_SOURCE_DIR}/data/JH-2013-set1.dat ${PROJECT_SOURCE_DIR}/data/steer_ep)
  CascadeTestTarget(DYPH PB-NLO-HERAI+II-2018-set2 ${PROJECT_SOURCE_DIR}/data/POWHEG-example.lhe.gz ${PROJECT_SOURCE_DIR}/data/steering-DY-PH.txt )
  CascadeTestTarget(DYPHhepmc23 PB-NLO-HERAI+II-2018-set2 ${PROJECT_SOURCE_DIR}/data/POWHEG-example.lhe.gz ${PROJECT_SOURCE_DIR}/data/steering-DY-PH-hepmc23.txt )
  SET_TESTS_PROPERTIES(testDYPHhepmc23 PROPERTIES ENVIRONMENT "PATH=${PROJECT_BINARY_DIR}/:$ENV{PATH};LD_LIBRARY_PATH=${PROJECT_BINARY_DIR}:$ENV{LD_LIBRARY_PATH};DYLD_LIBRARY_PATH=${PROJECT_BINARY_DIR}:$ENV{DYLD_LIBRARY_PATH};HEPMCOUT=DYPHhepmc23output.hepmc")
  if (CASCADE_RIVET AND RIVET_FOUND)
    #CascadeTestTarget(DYPHrivethepmc23 PB-NLO-HERAI+II-2018-set2 ${PROJECT_SOURCE_DIR}/data/POWHEG-example.lhe.gz ${PROJECT_SOURCE_DIR}/data/steering-DY-PH-rivethepmc23.txt )
    #SET_TESTS_PROPERTIES(testDYPHrivethepmc23 PROPERTIES ENVIRONMENT "PATH=${PROJECT_BINARY_DIR}/:$ENV{PATH};LD_LIBRARY_PATH=${PROJECT_BINARY_DIR}:$ENV{LD_LIBRARY_PATH};DYLD_LIBRARY_PATH=${PROJECT_BINARY_DIR}:$ENV{DYLD_LIBRARY_PATH};RIVET_ANALYSIS_PATH=$ENV{RIVET_ANALYSIS_PATH}:${RIVET_ANALYSIS_PATH};RIVET_DATA_PATH=$ENV{RIVET_DATA_PATH}:${RIVET_DATA_PATH}")
    CascadeTestTargetStandalone(epstandalone  PB-NLO-HERAI+II-2018-set2  ${PROJECT_SOURCE_DIR}/data/steering_ep.txt )  
    SET_TESTS_PROPERTIES(testepstandalone PROPERTIES     ENVIRONMENT "PATH=${PROJECT_BINARY_DIR}/:$ENV{PATH};LD_LIBRARY_PATH=${PROJECT_BINARY_DIR}:$ENV{LD_LIBRARY_PATH};DYLD_LIBRARY_PATH=${PROJECT_BINARY_DIR}:$ENV{DYLD_LIBRARY_PATH};HEPMCOUT=epstandaloneoutput.hepmc;RIVET_ANALYSIS_PATH=$ENV{RIVET_ANALYSIS_PATH}:${RIVET_ANALYSIS_PATH};RIVET_DATA_PATH=$ENV{RIVET_DATA_PATH}:${RIVET_DATA_PATH}")
  endif()
endif()




