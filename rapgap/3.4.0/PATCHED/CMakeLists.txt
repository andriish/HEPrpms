cmake_minimum_required(VERSION 3.1.0 FATAL_ERROR)
###########################Package@#####################################
project(cascade LANGUAGES C CXX Fortran )
set(PROJECT_VERSION 3.3.11)
SET(RAPGAP_VERSION 3.3.11)
SET(RAPGAP_VERSION_MAJOR  3)
SET(RAPGAP_VERSION_MINOR  3)
SET(RAPGAP_VERSION_PATCH  11)

set(CMAKE_VERBOSE_MAKEFILE ON)

###########Respect HFS, e.g. defines lib or lib64 when it is needed.####
include("GNUInstallDirs")

###########################Options #####################################
option(RAPGAP_BUILD_DOCS           "Enables building of documentation" OFF)
option(RAPGAP_EXTERNAL_PYTHIA6     "Enables usage of external Pythia6. Otherwise a local copy will be used. Debug-only, don't use in production!" OFF)
option(RAPGAP_EXTERNAL_ARIADNE     "Enables usage of external ARIADNE. Otherwise a local copy will be used. Debug-only, don't use in production!" OFF)
option(RAPGAP_ARIADNE               "Enables ARIADNE routines" ON)
option(RAPGAP_HEPMC2               "Enables HepMC2 routines" OFF)
option(RAPGAP_HEPMC3               "Enables HepMC3 routines" ON)
option(RAPGAP_RIVET                "Enables RIVET routines" ON)
option(RAPGAP_ENABLE_TESTS         "Enables tests" ON)
message(STATUS "RAPGAP: RAPGAP_BUILD_DOCS            ${RAPGAP_BUILD_DOCS}")
message(STATUS "RAPGAP: RAPGAP_EXTERNAL_PYTHIA6      ${RAPGAP_EXTERNAL_PYTHIA6}")
message(STATUS "RAPGAP: RAPGAP_EXTERNAL_ARIADNE      ${RAPGAP_EXTERNAL_ARIADNE}")
message(STATUS "RAPGAP: RAPGAP_ARIADNE               ${RAPGAP_ARIADNE}")
message(STATUS "RAPGAP: RAPGAP_HEPMC2                ${RAPGAP_HEPMC2}")
message(STATUS "RAPGAP: RAPGAP_HEPMC3                ${RAPGAP_HEPMC3}")
message(STATUS "RAPGAP: RAPGAP_RIVET                 ${RAPGAP_RIVET}")
message(STATUS "RAPGAP: RAPGAP_ENABLE_TESTS          ${RAPGAP_ENABLE_TESTS}")

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
message(STATUS "RAPGAP: Fortran_COMPILER_NAME=${Fortran_COMPILER_NAME}  CMAKE_Fortran_COMPILER_ID=${CMAKE_Fortran_COMPILER_ID}")
get_filename_component (CXX_COMPILER_NAME ${CMAKE_CXX_COMPILER} NAME)
message(STATUS "RAPGAP: CXX_COMPILER_NAME=${CXX_COMPILER_NAME}  CMAKE_CXX_COMPILER_ID=${CMAKE_CXX_COMPILER_ID}")
get_filename_component (C_COMPILER_NAME ${CMAKE_C_COMPILER} NAME)
message(STATUS "RAPGAP: C_COMPILER_NAME=${C_COMPILER_NAME}  CMAKE_C_COMPILER_ID=${CMAKE_C_COMPILER_ID}")
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
  message(STATUS "RAPGAP examples: Don't know how to set Fortran FLAGS for  ${Fortran_COMPILER_NAME} with ID ${Fortran_COMPILER_ID}")
endif ()
if(CMAKE_SYSTEM_NAME MATCHES Darwin)
  set(CMAKE_SHARED_LIBRARY_CREATE_Fortran_FLAGS "${CMAKE_SHARED_LIBRARY_CREATE_Fortran_FLAGS} -flat_namespace -single_module -undefined dynamic_lookup")
else()
endif()

######Mandatory External PACKAGES################################################
find_package(LHAPDF REQUIRED)
message(STATUS "RAPGAP: LHAPDF_VERSION=${LHAPDF_VERSION} LHAPDF_LIBRARIES=${LHAPDF_LIBRARIES} LHAPDF_INCLUDE_DIRS=${LHAPDF_INCLUDE_DIRS}")
if ( "${LHAPDF_VERSION}" VERSION_GREATER "6.0.0" ) 
#message(STATUS "RAPGAP: LHAPDF_VERSION=${LHAPDF_VERSION} LHAPDF_LIBRARIES=${LHAPDF_LIBRARIES} LHAPDF_INCLUDE_DIRS=${LHAPDF_INCLUDE_DIRS}")
   set(pdf_source rapgap/rystfu_lhapdf6.F h1qcd/pion_init-dummy.F h1qcd/pion_xpq-dummy.F  gluon/ryssha-dummy.F  gluon/d_xgx-dummy.F gammapdf/rystga-dummy.F)
else()
   set(pdf_source rapgap/rystfu_lhapdf5.F h1qcd/pion_init.F h1qcd/pion_xpq.F gluon/ryssha.F gluon/d_xgx.F gammapdf/rystga.F)
endif()

######OPTIONAL PACKAGES#################################################
if (RAPGAP_HEPMC2 AND RAPGAP_HEPMC3)
  message(STATUS "RAPGAP: Both options RAPGAP_HEPMC2 and RAPGAP_HEPMC3 were requsted. Only RAPGAP_HEPMC3 will be used. To use HEPMC2 library set -DRAPGAP_HEPMC3=OFF -DRAPGAP_HEPMC2=ON in the command line")
  set(RAPGAP_HEPMC2 OFF)
endif()
set(HEPMC_INTERFACE_DEFINITIONS "")
if (RAPGAP_HEPMC2)
  find_package(HepMC2 COMPONENTS FIO)
  #This is not default, to use HepMC2 we have to add this flag.
   list(APPEND HEPMC_INTERFACE_DEFINITIONS "PYTHIA6_USE_HEPMC2")
endif()
if (RAPGAP_HEPMC2 AND HepMC2_FOUND)
  message(STATUS "RAPGAP: HEPMC2_VERSION=${HEPMC2_VERSION} HEPMC2_LIBRARIES=${HEPMC2_LIBRARIES} HEPMC2_FIO_LIBRARIES=${HEPMC2_FIO_LIBRARIES} HEPMC2_INCLUDE_DIRS=${HEPMC2_INCLUDE_DIRS}")
endif()
if (RAPGAP_HEPMC3)
  find_package(HepMC3)
endif()
if (RAPGAP_HEPMC3 AND HepMC3_FOUND)
  message(STATUS "RAPGAP: HEPMC3_VERSION=${HEPMC3_VERSION} HEPMC3_LIBRARIES=${HEPMC3_LIBRARIES}  HEPMC3_INCLUDE_DIR=${HEPMC3_INCLUDE_DIR}") 
endif()
if (NOT HepMC2_FOUND) 
  list(APPEND HEPMC_INTERFACE_DEFINITIONS "DUMMYPYTHIA6TOHEPMC2")
endif()
if (NOT HepMC3_FOUND) 
  list(APPEND HEPMC_INTERFACE_DEFINITIONS "DUMMYPYTHIA6TOHEPMC3")
endif()
if (RAPGAP_RIVET)
  if (RAPGAP_HEPMC3 AND HepMC3_FOUND)
    find_package(Rivet COMPONENTS HEPMC3 )
  endif()
  if (RAPGAP_HEPMC2 AND HepMC2_FOUND)
    find_package(Rivet COMPONENTS HEPMC2 )
  endif()
  message(STATUS "RAPGAP: RIVET_FOUND=${RIVET_FOUND} RIVET_VERSION=${RIVET_VERSION} RIVET_LIBRARIES=${RIVET_LIBRARIES} RIVET_INCLUDE_DIRS=${RIVET_INCLUDE_DIRS} RIVET_DATA_PATH=${RIVET_DATA_PATH} RIVET_ANALYSIS_PATH=${RIVET_ANALYSIS_PATH} Rivet_HEPMC2_FOUND=${Rivet_HEPMC2_FOUND}  Rivet_HEPMC3_FOUND=${Rivet_HEPMC3_FOUND}")
  if ("${RIVET_VERSION}" VERSION_GREATER_EQUAL "4")
    add_definitions(-DRIVET4)
    add_definitions(-DRIVET_ENABLE_HEPMC_3)
  endif()
endif()

if (NOT RAPGAP_RIVET OR NOT RIVET_FOUND) 
  add_definitions(-DDUMMYRIVETINTERFACE)
endif()

set(conv_source rapgap/Pythia6ToHepMC.cc rapgap/convhepmc-helpers.F rapgap/RivetInterface.cc)
list(TRANSFORM conv_source PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/src/")

if (RAPGAP_EXTERNAL_PYTHIA6)
  find_package(Pythia6 REQUIRED)
  message(STATUS "RAPGAP: PYTHIA6_LIBRARIES=${PYTHIA6_LIBRARIES}")
endif()

if (RAPGAP_EXTERNAL_ARIADNE)
  find_package(Ariadne  REQUIRED)
  message(STATUS "RAPGAP: ARIADNE_LIBRARIES=${ARIADNE_LIBRARIES}")
endif()

if (RAPGAP_BUILD_DOCS)
  find_package(LATEX COMPONENTS PDFLATEX MAKEINDEX)
  message(STATUS "RAPGAP: LATEX_FOUND=${LATEX_FOUND} PDFLATEX_COMPILER=${PDFLATEX_COMPILER} MAKEINDEX_COMPILER=${MAKEINDEX_COMPILER}")
endif()

##RPATHS ###############################################################
#Adding extra RPATHS for Darwin. Only if those are not system directories and do exist.
if(CMAKE_SYSTEM_NAME MATCHES Darwin)
  SET(RAPGAP_RPATHS  "${LHAPDF_LIBRARY_DIR}" "${HEPMC2_LIBRARY_DIR}" "${HEPMC3_LIBRARY_DIR}" "${ARIANDE_LIBRARY_DIR}" "${PYTHIA6_LIBRARY_DIR}")
  list(REMOVE_DUPLICATES RAPGAP_RPATHS)
  foreach(implicitdir ${CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES})
    LIST(REMOVE_ITEM RAPGAP_RPATHS ${implicitdir})
  endforeach()
  list(REMOVE_DUPLICATES RAPGAP_RPATHS)
  foreach( rdir ${RAPGAP_RPATHS} )
    if (EXISTS ${rdir} AND IS_DIRECTORY ${rdir} AND (NOT "${rdir}" STREQUAL ""))
     LIST(APPEND CMAKE_INSTALL_RPATH  ${rdir})
    endif()
  endforeach()
  message(STATUS "RAPGAP: CMAKE_INSTALL_RPATH            ${CMAKE_INSTALL_RPATH}")
ENDIF()
##BASES ################################################################
set(librapgapbases_la_SOURCES  
                     bases.f  bschck.f  bsgetw.f  bsintg.f  bsputw.f 
                     bswrit.f drnset.f  shplot.f  sphist.f  xhfill.f 
                     xhscle.f bhinit.f  bsdate.f  bsgrid.f  bslist.f 
                     bsread.f dhfill.f  shrset.f  spinfo.f 
                     xhinit.f bhplot.f  bsdims.f   bsordr.f 
                     bstcnv.f dhinit.f  shcler.f  shupdt.f  sprgen.f 
                     xhordr.f bhrset.f  bsetgu.f  bsinfo.f  bsparm.f 
                     bstime.f dhplot.f  shfill.f  spchck.f  spring.f 
                     xhplot.f bhsave.f  bsetgv.f  bsinit.f  bsprnt.f 
                     bsutim.f drn.f     shinit.f   xhchck.f 
                     xhrnge.f
 )
list(TRANSFORM librapgapbases_la_SOURCES PREPEND "${PROJECT_SOURCE_DIR}/bases51/")
add_library(rapgapbases SHARED ${librapgapbases_la_SOURCES})
target_include_directories(rapgapbases PRIVATE ${PROJECT_SOURCE_DIR}/include)
install(TARGETS rapgapbases DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT libs)
set_target_properties(rapgapbases PROPERTIES LINKER_LANGUAGE Fortran SOVERSION 0)

##MYCERN ###############################################################
set(rapgapmycern_la_SOURCES  abend.F  besi0.F  expint.F  ranlux.F  rnpssn.F  
                       ucopy.F  vzero.F dgauss.F rnormx.F 
                       datime.F timex.F  timest.F timed.F fint.F 
                       rgquad.F d107r1.F dgquad.F d107d1.F divdif.F 
                       binomi.F kerset.F
                         )
list(TRANSFORM rapgapmycern_la_SOURCES PREPEND "${PROJECT_SOURCE_DIR}/misc/")
add_library(rapgapmycern SHARED ${rapgapmycern_la_SOURCES} src/rapgap/rndm.F src/rapgap/draprn.F  )
target_include_directories(rapgapmycern PRIVATE ${PROJECT_SOURCE_DIR}/include)
install(TARGETS rapgapmycern DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT libs)
set_target_properties(rapgapmycern PROPERTIES LINKER_LANGUAGE Fortran SOVERSION 0)

target_link_libraries(rapgapbases rapgapmycern)

#PYTHIA6 ###############################################################
if (NOT RAPGAP_EXTERNAL_PYTHIA6)
  set( libpythia_la_SOURCES  
             fhhiggscorr.f  pyct5l.f  pygfxx.f  pylamf.f  pypole.f  pysfdc.f  pytime.f 
             fhsetflags.f   pyct5m.f       pyggam.f       pyldcm.f       pyprep.f       pysgex.f       pytune.f 
             fhsetpara.f    pycteq.f       pygive.f       pylhef.f       pyptdi.f       pysghf.f       pyuedc.f
             pycttr.f       pyglui.f       pylist.f       pyptfs.f       pysghg.f       pyupda.f
             py1ent.f       pydcyk.f       pygram.f       pylogo.f       pyptis.f       pysgqc.f       pyupev.f
             py2ent.f       pydecy.f       pygraw.f       pymael.f       pyptmi.f       pysgsu.f       pyupin.f
             py2frm.f       pydiff.f       pygrvd.f       pymass.f       pyqqbh.f       pysgtc.f       pyupre.f
             py3ent.f       pydisg.f       pygrvl.f       pymaxi.f       pyr.f          pysgwz.f       pyveto.f
             py4ent.f       pydocu.f       pygrvm.f       pymemx.f       pyradk.f       pyshow.f       pywaux.f
             py4frm.f       pydump.f       pygrvs.f       pymewt.f       pyrand.f       pysigh.f       pywdkk.f
             py4jet.f       pyedit.f       pygrvv.f       pymign.f       pyreco.f       pysimp.f       pywidt.f
             py4jts.f       pyeevt.f       pygrvw.f       pymihg.f       pyremn.f       pyslha.f       pywidx.f
             py4jtw.f       pyeicg.f       pygvmd.f       pymihk.f       pyresd.f       pyspen.f       pyx2xg.f
             py6frm.f       pyeig4.f       pyh2xx.f       pymirm.f       pyrget.f       pysphe.f       pyx2xh.f
             pyadsh.f       pyerrm.f       pyhepc.f       pymrun.f       pyrghm.f       pyspli.f       pyx3jt.f
             pyalem.f       pyevnt.f       pyhext.f       pymsin.f       pyrnm3.f       pysspa.f       pyx4jt.f
             pyalps.f       pyevnw.f       pyhfth.f       pymult.f       pyrnmq.f       pystat.f       pyxdif.f
             pyangl.f       pyevol.f       pyhggm.f       pyname.f       pyrobo.f       pystbh.f       pyxdin.f
             pyapps.f       pyevwt.f       pyhist.f       pynjdc.f       pyrset.f       pystop.f       pyxjet.f
             pybesq.f       pyexec.f       pyi3au.f       pynmes.f       pyrvch.f       pystrf.f       pyxkfl.f
             pybksb.f       pyfact.f       pyinbm.f       pynull.f       pyrvg1.f       pysubh.f       pyxtee.f
             pyboei.f       pyfcmp.f       pyindf.f       pyofsh.f       pyrvg2.f       pysugi.f       pyxtot.f
             pybook.f       pyfeyn.f       pyinit.f       pyonia.f       pyrvg3.f       pytabu.f       pyxued.f
             pycba2.f       pyfill.f       pyinki.f       pyonof.f       pyrvg4.f       pytaud.f       pyxxga.f
             pycbal.f       pyfint.f       pyinom.f       pyoper.f       pyrvgl.f       pytbbc.f       pyxxz6.f
             pycdiv.f       pyfisb.f       pyinpr.f       pyp.f          pyrvgw.f       pytbbn.f       pyzdis.f
             pycell.f       pyfowo.f       pyinre.f       pypcmp.f       pyrvi1.f       pytbdy.f       ssmssm.f
             pychge.f       pyfram.f       pyjmas.f       pypdel.f       pyrvi2.f       pytbhb.f       
             pycjdc.f       pyfscr.f       pyjoin.f       pypdfl.f       pyrvi3.f       pytbhg.f       
             pyckbd.f       pygaga.f       pyjurf.f       pypdfu.f       pyrvne.f       pytbhq.f       sugra.f
             pyclus.f       pygamm.f       pyk.f          pypdga.f       pyrvr.f        pytbhs.f       
             pycmq2.f       pygano.f       pykcut.f       pypdpi.f       pyrvs.f        pytecm.f       
             pycmqr.f       pygau2.f       pykfdi.f       pypdpo.f       pyrvsb.f       pytest.f       
             pycomp.f       pygaus.f       pykfin.f       pypdpr.f       pyrvsf.f       pythag.f       visaje.f
             pycrth.f       pygbeh.f       pyklim.f       pypile.f       pysave.f       pythrg.f
             pycsrt.f       pygdir.f       pykmap.f       pyplot.f       pyscat.f       pythru.f
             upveto.f        upinit.f       upevnt.f
  )
  list(TRANSFORM libpythia_la_SOURCES PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/pythia6428/")
  add_library(rapgappythia6 SHARED ${libpythia_la_SOURCES})
  set_target_properties(rapgappythia6 PROPERTIES LINKER_LANGUAGE Fortran SOVERSION 0)
  target_link_libraries(rapgappythia6 ${LHAPDF_LIBRARIES})
  set(PYTHIA6_LIBRARIES rapgappythia6)
  install(TARGETS rapgappythia6 DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT libs)
endif()
##ARIADNE ###############################################################
if (NOT RAPGAP_EXTERNAL_ARIADNE)
  set( librapgapariadne_la_SOURCES  
                arcopa.f        arflam.f     arnofl.f        arradg.f        arstra.f        atest.f 
		arcrdi.f        argdis.f        arrado.f        diboos.f
		arging.f        arorie.f        arradp.f        diclus.f
		argoni.f        arpars.f        arradq.f        arsume.f        dijoin.f
		ar4ent.f        argpt2.f        arreca.f        arsuve.f        dirphi.f
		ar4frm.f        argqcd.f        aript2.f        arpcms.f        arreco.f        dirthe.f
		araddg.f        ardilg.f        argqed.f        arlept.f        arphas.f        arrecs.f        arthrw.f
		aradig.f        argtyp.f        arpoki.f        arremn.f        artune.f        ldcexe.f        pystfu.f
		aralps.f        ardump.f        armade.f        arrflv.f        arumec.f
		arangl.f        arduph.f        ariach.f        armael.f        arposf.f        arrndx.f        arundo.f
		arbocm.f        ardyre.f        armass.f        arprda.f        arrndy.f        arupom.f
		arbook.f        arearr.f        armcdi.f        arprob.f        arrobo.f        aruthr.f
		arcasc.f        areeme.f        ariatn.f        armecu.f        arptca.f        arscan.f        arveto.f
		archem.f        aremit.f        arildc.f        armeps.f        arptqq.f        arsngl.f        arwght.f        lnstrf.f
		archki.f        arerrm.f        armipt.f        arputr.f        arsplg.f        arx3jt.f
		arclus.f        arexec.f        arinit.f        armtqq.f        arpyth.f        arx4jt.f
		arcomp.f        arexma.f        arinqq.f        arniqq.f        arqq2o.f        arstor.f        arzcms.f artest.f
)
  list(TRANSFORM librapgapariadne_la_SOURCES PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/ariadne412/")
  add_library(rapgapariadne SHARED ${librapgapariadne_la_SOURCES}   src/rapgap/pydata.f)  
  set_target_properties(rapgapariadne PROPERTIES LINKER_LANGUAGE Fortran SOVERSION 0)
   target_link_libraries(rapgapariadne ${PYTHIA6_LIBRARIES})
  set(ARIADNE_LIBRARIES rapgapariadne)
  install(TARGETS rapgapariadne DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT libs)
endif()
if (RAPGAP_ARIADNE)
  set( ariadne_SOURCES  arexec.F arinit.F )
  list(TRANSFORM ariadne_SOURCES PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/src/ariadne/")
else()
  set( ariadne_SOURCES  arexec.f arinit.f )
  list(TRANSFORM ariadne_SOURCES PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/src/ariadne-dummy/")
  set(ARIADNE_LIBRARIES )
endif()

#RAPGAP3###############################################################
set(librapgap33_sources 
        rapgap/aalam.F    rapgap/lhaini.F    rapgap/rgstga.F  
        rapgap/alphas.F   rapgap/eleqcdc.F   rapgap/lmeps.F     rapgap/pytcha.F  
        rapgap/eleqq.F    rapgap/lmeps99.F   rapgap/qcdgrid.F   rapgap/upveto.F  
        rapgap/eleqqb.F   rapgap/lprikt.F    rapgap/qcdgridi.F  
        rapgap/aricha.F   rapgap/eleqqf.F    rapgap/lremh.F     rapgap/qcdmix.F  
        rapgap/eleqqg.F   rapgap/lscale.F    rapgap/raend.F  
        rapgap/blepin.F   rapgap/eleqqgg.F   rapgap/ralmkt.F  
        rapgap/colres.F   rapgap/eleqql.F    rapgap/partdf.F    rapgap/rapcha.F  
        rapgap/cutg.F     rapgap/eleres.F    rapgap/partdh.F    rapgap/rapgap.F  
        rapgap/dfridr.F   rapgap/elqcdcnl.F  rapgap/partdi.F    rapgap/raphi.F  
        rapgap/dfun.F     rapgap/elqqfnl.F   rapgap/partdi99.F  rapgap/rastfu.F  
        rapgap/dhformf.F  rapgap/elqqlnl.F   rapgap/parths.F    rapgap/rat2di.F  
        rapgap/diffr1.F   rapgap/event.F     rapgap/parti.F     rapgap/rghaprep.F  
        rapgap/diffr2.F   rapgap/eventep.F   rapgap/pdfgrid.F     
        rapgap/diffr3.F   rapgap/eventpp.F   rapgap/phase.F     rapgap/rgq2w2mx.F  
        rapgap/diffr30.F  rapgap/pomstr.F    rapgap/rgsatrap.F  
        rapgap/diffr4.F   rapgap/fxn1.F      rapgap/ppmeps.F    rapgap/rgsatrev.F  
        rapgap/diffr5.F   rapgap/fxnb.F      rapgap/pqcd.F      rapgap/rgupevnt.F  
        rapgap/diffr6.F   rapgap/gadap.F     rapgap/pqcdi.F     rapgap/rgupinit.F  
        rapgap/diffr7.F   rapgap/gadap2.F    rapgap/pqcdqqb.F   rapgap/rlu.F  
        rapgap/diffr8.F   rapgap/gadapf.F    rapgap/pqpm.F      
        rapgap/dir1.F     rapgap/graini.F    rapgap/prodiff.F    
        rapgap/dir2.F     rapgap/grawq2.F    rapgap/ptime.F     rapgap/steer.F  
        rapgap/dir3.F     rapgap/h1rn.F      rapgap/rggamm.F    rapgap/usdiffr.F  
        rapgap/dir4.F     rapgap/hercha.F    rapgap/rgremn.F    rapgap/xpq30.F  
        rapgap/dirh0.F    rapgap/eleh0.F     rapgap/dirxv1.F      rapgap/elexv1.F  
        rapgap/dirvqq.F   rapgap/elevqq.F  
        rapgap/dirxvq.F   rapgap/elevqg.F  
        rapgap/difxvq.F   rapgap/difxv1.F  
        rapgap/dir99.F    rapgap/xpqhf.F  
        rapgap/dot.F      rapgap/rasspa.F  
        rapgap/dot1.F     rapgap/rgstfe.F    
        rapgap/rapgen.F   rapgap/intga.F     rapgap/rgstfu.F  
        util/Ctq4Pdfm.F   util/Ctq6Pdfm.F   util/minuts.F  
        util/Ctq5Pdfm.F   util/getintPDF.F  util/strpriv.F  
        actw/actwfit.F    actw/evlrnfe.F  actw/pardis.F   
        actw/ratint.F     actw/warnr.F  
        actw/evlrd.F      actw/nextun.F   actw/polint.F  actw/rdupd.F  
        gammapdf/grspar.F gammapdf/sasgam.F  
        gluon/blueml.F    gluon/ccfmstfu.F  gluon/ktstfu.F  gluon/runiglu.F  
        gluon/dhfint.F  
        h1qcd/fluxh1.F  h1qcd/i_2006_fitb.F  h1qcd/i_nlo_q3g1.F  h1qcd/qcd_2006.F  
        h1qcd/h12006flux.F   h1qcd/i_lo_q3g0.F    h1qcd/i_nlo_q3g3.F  h1qcd/xyinter.F  
        h1qcd/h1qcd2006.F    h1qcd/i_lo_q3g1.F   
        h1qcd/h1qcdfit.F     h1qcd/i_lo_q3g3.F   
        h1qcd/i_2006_fita.F  h1qcd/i_nlo_q3g0.F   h1qcd/qcd_1994.F  
        hardpom/dgrv_nln.F  hardpom/f2dhw.F  hardpom/f2md.F  
        hardpom/f2blw.F     hardpom/f2mcd.F  
        nikzak/nikzak.F  
        pdiss/decdel.F  pdiss/decrho.F  pdiss/pdisdcvm.F  pdiss/rgrambo.F  
        pdiss/deceta.F  pdiss/fragpj.F  pdiss/pdismass.F   pdiss/rgrng.F  
        pdiss/decnst.F  pdiss/fragpx.F  pdiss/prodiffvm.F  pdiss/rkno.F  
        pdiss/decpi0.F  pdiss/pdisdc.F  pdiss/ranbw.F      pdiss/splitp.F  
        rho/dboost.F  rho/elerho.F  rho/gdeca3.F  rho/gensph.F  rho/p0togg.F  
        satrap/compcross.F  satrap/phifunc.F    satrap/satsigtot.F satrap/swavet.F  
        satrap/fourdot.F    satrap/ran2body.F   satrap/saturpar.F   satrap/threedot.F  
        satrap/inputkine.F  satrap/ran3body.F   satrap/sigdif.F  
        satrap/intwave.F    satrap/ranq2w2mx.F  satrap/swaveg.F  
        satrap/partkine.F   satrap/ranuni.F     satrap/swavel.F  
        sca/sca_dat.F  sca/sca_main.F  sca/sca_pdf.F  sca/scapdf.F  
        heracles466/brasse.f     heracles466/hsclm2.f  heracles466/hsgenm.f  heracles466/hssign.f  
        heracles466/d01ajf.f     heracles466/hsclm3.f  heracles466/hsglow.f  heracles466/hsspen.f  
        heracles466/dfnc00.f     heracles466/hsclm4.f  heracles466/hsgupp.f  heracles466/hssrgg.f  
        heracles466/dfncii.f     heracles466/hscln.f   heracles466/hshadq.f  heracles466/hssrgz.f  
        heracles466/dialfs.f     heracles466/hscmw.f   heracles466/hsinil.f  heracles466/hssrww.f  
        heracles466/diflop.f     heracles466/hscmwz.f  heracles466/hsinit.f  heracles466/hssrzz.f  
        heracles466/djgevt.F     heracles466/hscphl.f  heracles466/hsiwz1.f  heracles466/hsstal.f  
        heracles466/djgvar.F     heracles466/hscwll.f  heracles466/hsiwz2.f  heracles466/hsstbk.f  
        heracles466/dx1fcf.f     heracles466/hscwlr.f  heracles466/hsixy.f   heracles466/hsstdl.f  
        heracles466/f2allm.f     heracles466/hscwql.f  heracles466/hsk1k3.f  heracles466/hsstr1.f  
        heracles466/fiuser.f     heracles466/hscwqr.f  heracles466/hsk1ts.f  heracles466/hsstrf.f  
        heracles466/flgint.F     heracles466/hscxsm.f  heracles466/hsl1k3.f  heracles466/hsterm.F  
        heracles466/flintg.f     heracles466/hsd0.f    heracles466/hsl1ts.f  heracles466/hstpar.f  
        heracles466/flipol.f     heracles466/hsd13c.f  heracles466/hsloqs.f  heracles466/hstrit.f  
        heracles466/flqint.F     heracles466/hsdelo.f  heracles466/hslts1.f  heracles466/hstsk1.f  
        heracles466/fltabl.f     heracles466/hsdelx.f  heracles466/hslts2.f  heracles466/hstsk2.f  
        heracles466/fltint.f     heracles466/hsdout.f  heracles466/hslufl.f  heracles466/hsuser.F  
        heracles466/hsdqds.f  heracles466/hslzk1.f  heracles466/hswcut.F  
        heracles466/gausk1.f     heracles466/hsdqdv.f  heracles466/hslzk2.f  heracles466/hswg1l.f  
        heracles466/gausk2.f     heracles466/hsdsgq.f  heracles466/hslzk3.f  heracles466/hswg1r.f  
        heracles466/heracl466.F  heracles466/hsdtin.f  heracles466/hslzts.f  heracles466/hswg2l.f  
        heracles466/hs46ini.F    heracles466/hsduwa.f  heracles466/hswg2r.f  
        heracles466/hsacpt.F     heracles466/hsduwq.f  heracles466/hsnc22.f  heracles466/hswgtx.f  
        heracles466/hsbcga.f     heracles466/hsel22.f  heracles466/hsncg1.f  heracles466/hswpdf.f  
        heracles466/hsbkin.f     heracles466/hselco.f  heracles466/hsncg2.f  heracles466/hswrpa.f  
        heracles466/hselg1.f  heracles466/hsnrat.f  heracles466/hswrsa.f  
        heracles466/hsbrnc.f     heracles466/hselg2.f  heracles466/hsomax.f  heracles466/hsxmax.f  
        heracles466/hsbxca.f     heracles466/hselk1.f  heracles466/hsomin.f  heracles466/hsxsct.f  
        heracles466/hsbxcv.f     heracles466/hselk2.f  heracles466/hspgfx.f  heracles466/hsxsmn.f  
        heracles466/hsbxi0.f     heracles466/hsenuw.f  heracles466/hsprlg.f  heracles466/lnstrf.F  
        heracles466/hsbxi5.f     heracles466/hsestm.f  heracles466/hspver.f  heracles466/ltimex.F  
        heracles466/hscc22.f     heracles466/hsevtg.f  heracles466/hsrndm.f  heracles466/lystfu.F  
        heracles466/hsccba.f     heracles466/hsf2bk.f  heracles466/hsrnin.f  heracles466/parinv.f  
        heracles466/hsccbq.f     heracles466/hsf2dl.f  heracles466/hsrnou.f  heracles466/pepevt.F  
        heracles466/hsccg1.f     heracles466/hsfcms.f  heracles466/hsrnst.f  heracles466/rf12.f  
        heracles466/hsccg2.f     heracles466/hsfhfb.f  heracles466/hsrnte.f  heracles466/rslac.f  
        heracles466/hscckl.f     heracles466/hsfie0.f  heracles466/hssab0.f  heracles466/sfecfe.f  
        heracles466/hsccms.f     heracles466/hsfiel.f  heracles466/hssab1.f  heracles466/sigvmes.f  
        heracles466/hsccqf.f     heracles466/hsfiv1.f  heracles466/hssetf.f  heracles466/stein.f  
        heracles466/hsccqi.f     heracles466/hsfiv2.f  heracles466/hssetp.f  heracles466/strfbs.f  
        heracles466/hsccsa.f     heracles466/hsfiv3.f  heracles466/hssfgz.f  heracles466/svtot.f  
        heracles466/hsccsq.f     heracles466/hsfivc.f  heracles466/hssfww.f  heracles466/vegas.f  
        heracles466/hscir.f      heracles466/hsfivm.f  heracles466/hssfzz.f  heracles466/vmesnuc.f  
        heracles466/hsckmx.f     heracles466/hsflab.f  heracles466/hssgcc.f  heracles466/wabc.f  
        heracles466/hsclab.f     heracles466/hsflav.f  heracles466/hssgel.f  
        heracles466/hsclm1.f     heracles466/hsfone.f  heracles466/hssgnc.f  
        sophia466/breitwigner.f   sophia466/icon_sib_pdg.f  sophia466/probangle.f  
        sophia466/check_event.f   sophia466/initial.f       sophia466/proc_twopart.f  
        sophia466/crossection.f   sophia466/lund_frag.F     sophia466/res_decay3.f  
        sophia466/lund_get.F      sophia466/dec_inter3.f    sophia466/lund_put.F  
        sophia466/dec_proc2.f      
        sophia466/dec_res2.f      sophia466/pl.f            sophia466/scatangle.f  
        sophia466/decpar.f        sophia466/po_altra.f      sophia466/singleback.f  
        sophia466/decsib.f        sophia466/po_mshell.f       
        sophia466/djgnoe.F        sophia466/po_rndbet.f     sophia466/sophia.f  
        sophia466/djgsph.F        sophia466/po_rndgam.f     sophia466/twoback.f  
        sophia466/ef.f            sophia466/po_selsx2.f     sophia466/valences.f  
        sophia466/gamma_h.f       sophia466/po_trans.f   
        sophia466/gauss.f         sophia466/po_xlam.f  
        sophia466/icon_pdg_sib.f  sophia466/print_event.f   
        ${pdf_source}   
        )
list(TRANSFORM librapgap33_sources PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/src/")
add_library(rapgap33 SHARED ${librapgap33_sources})
target_include_directories(rapgap33 PRIVATE ${PROJECT_SOURCE_DIR}/include)

target_link_libraries(rapgap33 PRIVATE rapgapbases rapgapmycern ${LHAPDF_LIBRARIES})
target_link_libraries(rapgap33 PRIVATE ${PYTHIA6_LIBRARIES} ${ARIADNE_LIBRARIES})
install(TARGETS rapgap33 DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT libs)
set_target_properties(rapgap33 PROPERTIES LINKER_LANGUAGE Fortran SOVERSION 0)



##RAPGAP BIN ##########################################################
set(rapgap33_SOURCES  
            rapgap/rgmain-hepmc.F  rapgap/drn.F  rapgap/pyr.F rapgap/pyname.F 
            rapgap/upinit.F   rapgap/upevnt.F  )
list(TRANSFORM rapgap33_SOURCES PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/src/")
add_executable(rapgap ${rapgap33_SOURCES} ${ariadne_SOURCES} ${conv_source})
target_include_directories(rapgap PRIVATE ${PROJECT_SOURCE_DIR}/include   ${LHAPDF_INCLUDE_DIR})
target_link_libraries(rapgap PRIVATE rapgap33  rapgapbases rapgapmycern  ${LHAPDF_LIBRARIES})
if (RAPGAP_ARIADNE)
  target_link_libraries(rapgap PRIVATE ${ARIADNE_LIBRARIES})
endif()
set_target_properties(rapgap PROPERTIES OUTPUT_NAME rapgap33 LINKER_LANGUAGE Fortran)
target_link_libraries(rapgap PRIVATE ${PYTHIA6_LIBRARIES} ${ARIADNE_LIBRARIES})
install(TARGETS rapgap DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT binary)
set_target_properties(rapgap PROPERTIES OUTPUT_NAME "rapgap33")


if ( (RAPGAP_HEPMC2 AND HepMC2_FOUND)  OR  (RAPGAP_HEPMC3 AND HepMC3_FOUND) )
  if (HepMC2_FOUND)
    target_include_directories(rapgap PRIVATE  ${HEPMC2_INCLUDE_DIRS})
    target_link_libraries(rapgap PRIVATE ${HEPMC2_LIBRARIES} ${HEPMC2_FIO_LIBRARIES})
    set_property(TARGET rapgap PROPERTY CXX_STANDARD 11) # We need 03
  endif()
  if (HepMC3_FOUND)
    target_include_directories(rapgap PRIVATE  ${HEPMC3_INCLUDE_DIR})
    target_link_libraries(rapgap PRIVATE ${HEPMC3_LIBRARIES})
    set_property(TARGET rapgap PROPERTY CXX_STANDARD 11)
  endif()
  target_compile_definitions(rapgap PRIVATE $<$<COMPILE_LANGUAGE:CXX>:${HEPMC_INTERFACE_DEFINITIONS}>)
endif()
if (RAPGAP_RIVET AND RIVET_FOUND)
  target_include_directories(rapgap PRIVATE  ${RIVET_INCLUDE_DIR}) 
  target_link_libraries(rapgap PRIVATE ${RIVET_LIBRARIES})
  set_property(TARGET rapgap PROPERTY CXX_STANDARD 17)
endif()

## MANUAL  #############################################################
if (RAPGAP_BUILD_DOCS AND LATEX_FOUND AND LATEX_PDFLATEX_FOUND AND LATEX_MAKEINDEX_FOUND)
  set( manual_sources 
       ${PROJECT_SOURCE_DIR}/manual/rapgap32.tex 
       ${PROJECT_SOURCE_DIR}/manual/mcite.sty
       ${PROJECT_SOURCE_DIR}/manual/dina4.sty
       ${PROJECT_SOURCE_DIR}/manual/dismc.epsi
       ${PROJECT_SOURCE_DIR}/manual/difmc.epsi
     )
  file(COPY ${manual_sources} DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/manual)
  ADD_CUSTOM_TARGET(   latexcompileall
                       COMMAND ${PDFLATEX_COMPILER} rapgap32
                       COMMAND ${PDFLATEX_COMPILER} rapgap32
                       COMMAND ${MAKEINDEX_COMPILER} rapgap32
                       COMMAND ${PDFLATEX_COMPILER} rapgap32
                       COMMENT "Creating manual "
                       WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/manual
                       DEPENDS ${PROJECT_SOURCE_DIR}/manual/rapgap32.tex
                      )
  ADD_CUSTOM_TARGET(all-formats ALL) # Entry point of execution.
  ADD_DEPENDENCIES( all-formats latexcompileall)
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/manual/rapgap32.pdf DESTINATION  ${CMAKE_INSTALL_DOCDIR} COMPONENT doc)
endif()
### DATA ###############################################################
set(dataexamples
data/ccfm-gam.dat                            data/steer-ep-diff                           
data/ccfm-ipgg0ns-1-0-all-lhc.dat            data/steer-ep-resgamma
data/ccfm-ipgg1ns1-0-all-lhc.dat             data/steer-ep-f2c 
data/steer-ep-no-qedrad-dis-rivethepmc23 
data/ccfm-final-setA0+.dat                   data/ccfm-ipgg2ns2-0-all-lhc.dat
data/ccfm-final-setA0-.dat                   data/ccfm-setC.dat                           data/steer-ep-ipro20                         data/steer-pp
data/ccfm-final-setA0.dat                    data/ccfm.dat                                data/steer-ep-ipro20.bck                     data/steer-pp-rapidity-gap
data/ccfm-final-setA1.dat                    data/kmr.dat                                 data/steer-ep-no-qedrad-dis
data/ccfm-final-setB0+.dat                   data/kms.dat                                 data/steer-ep-no-qedrad-dis-ipro12           data/steer-pp-z0
data/ccfm-final-setB0-.dat                   data/steer-ep						data/steer-ep-ariadne
data/ccfm-final-setB0.dat                    data/steer-ep-charm
data/ccfm-final-setB1.dat                    data/steer-ep-qedrad-dis
)
list(TRANSFORM dataexamples PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/")
install(FILES ${dataexamples} DESTINATION  ${CMAKE_INSTALL_DATADIR}/rapgap COMPONENT doc)
install(DIRECTORY include/ DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/rapgap COMPONENT devel
FILES_MATCHING PATTERN "*"
          PATTERN "*akefile*" EXCLUDE
          PATTERN "*bak" EXCLUDE
)

macro(RapgapTestTarget nam pdf card hmout)
    add_custom_target(test${nam}${pdf} ALL COMMAND lhapdf install ${pdf})
    add_test(NAME test${nam} COMMAND sh -c "${PROJECT_BINARY_DIR}/rapgap33 < ${card}"  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
    if (RAPGAP_RIVET AND RIVET_FOUND)
      SET_TESTS_PROPERTIES(test${nam} PROPERTIES     ENVIRONMENT "PATH=${PROJECT_BINARY_DIR}/:$ENV{PATH};LD_LIBRARY_PATH=${PROJECT_BINARY_DIR}:$ENV{LD_LIBRARY_PATH};DYLD_LIBRARY_PATH=${PROJECT_BINARY_DIR}:$ENV{DYLD_LIBRARY_PATH};HEPMCOUT=${hmout};RIVET_ANALYSIS_PATH=$ENV{RIVET_ANALYSIS_PATH}:${RIVET_ANALYSIS_PATH};RIVET_DATA_PATH=$ENV{RIVET_DATA_PATH}:${RIVET_DATA_PATH}")
    else()
      SET_TESTS_PROPERTIES(test${nam} PROPERTIES     ENVIRONMENT "PATH=${PROJECT_BINARY_DIR}/:$ENV{PATH};LD_LIBRARY_PATH=${PROJECT_BINARY_DIR}:$ENV{LD_LIBRARY_PATH};DYLD_LIBRARY_PATH=${PROJECT_BINARY_DIR}:$ENV{DYLD_LIBRARY_PATH};HEPMCOUT=${hmout}")
    endif()
endmacro(RapgapTestTarget)

if(RAPGAP_ENABLE_TESTS)
  ENABLE_TESTING()
  if (RAPGAP_RIVET AND RIVET_FOUND)
    RapgapTestTarget(eprivet CT10 ${PROJECT_SOURCE_DIR}/data/steer-ep-no-qedrad-dis-rivethepmc23  disoutrivet)
  endif()
  RapgapTestTarget(ep CT10 ${PROJECT_SOURCE_DIR}/data/steer-ep-no-qedrad-dis  disout)
endif()


