## Copyright (C) 2006 Free Software Foundation, Inc.
## This Makefile.am is free software; the Free Software Foundation
## gives unlimited permission to copy, distribute and modify it.
AUTOMAKE_OPTIONS = foreign
AUTOMAKE_OPTIONS += subdir-objects

AM_FFLAGS = -I$(srcdir)/../include 
AM_FFLAGS += $(MYFFLAGS)
AM_CPPFLAGS = -I$(srcdir)/../include 
AM_CPPFLAGS+= -I$(HEPMC)/include  
AM_CPPFLAGS+= -I$(PYTHIA8_DIR)/include  
#AM_CPPFLAGS+= -I$(BOOST_CFLAGS)
#AM_CPPFLAGS+= $(ROOT_CFLAGS)
#AM_CPPFLAGS+= -pthread -std=c++17 -m64 -I$(ROOT_INCDIR)


bin_PROGRAMS = cascade
cascade_SOURCES =  casmain.F  drn.F  upevnt.F upinit.F pydata.F
#cascade_FFLAGS = -I$(top_srcdir)/include -I$(PYTHIA_SOURCE) $(MYFFLAGS) 
#cascade_FFLAGS = -I$(top_srcdir)/include -I$(PYTHIA_SOURCE) $(MYFFLAGS) 

conv_source=convhepmc-helpers.F  Pythia6ToHepMC2.cc  Pythia6ToHepMC3.cc RivetInterface.cc
if NO_HEPMC
AM_CPPFLAGS+=-DDUMMYPYTHIA6TOHEPMC2
endif
if NO_HEPMC3
AM_CPPFLAGS+=-DDUMMYPYTHIA6TOHEPMC3
endif
if NO_RIVET
AM_CPPFLAGS+=-DDUMMYRIVETINTERFACE
endif

if NO_TMDLIB
   tmdlib_source=tmdlib-dummy.F
endif

if NO_PYTHIA8
   pythia8_source=pythia8frag-dummy.cc
else
   pythia8_source=pythia8frag.cc
endif

if TMDLIB_EW
   casc_source = casTMD.F
else
   casc_source = casTMDnoEW.F
endif

lib_LTLIBRARIES = libcascade3.la 

libcascade3_la_SOURCES = aalam.F    caps.F      cascha.F    caupevnt.F  dfun.F   \
                   event.F    mscatt.F     steer.F   xsecpsi.F   \
                   alphas.F   caps_ms.F   cascps.F    caupinit.F  dgrv_nln.F \
                   fxn1.F     multscatt.F  pyr.F ccfmstfq.F   \
                   bcasin.F   casgen.F    ccfmstfg.F  dot1.F  \
                   fxnb.F     parti.F      pytcha.F  \
                   blueml.F   caremh.F    casini.F    ccfmstfu.F  dot.F  \
                   ktstfu.F   p_gamgam.F   xglu.F \
                   caend.F    caremn.F     cutg.F     \
                   mehiggs.F  phase.F      xgx.F \
                   caprikt.F  casbran.F   caspli.F    dcasrn.F    d_xgx.F \
                   meoffsh.F  p_mscatt.F   ranums.F  xhiggs.F \
                   capsee.F   cascade.F   cauniglu.F  dfridr.F    elemnz.F \
                   minuts.F   p_semih.F    ryssha.F  xsec1.F xsecqg.F  \
                   meoffjpsi.F  dhfint.F   \
			 colorflow.F xvq.F mevq.F meqg.F pyadsh_cas.F \
                   mcint.F mcgen.F \
                   xvqq.F mevqq.F ktOrtAmVqNew.F \
                   xsecchi.F meoffchi.F mev.F xv1.F \
                   xsecjj.F meoffjj.F jj_ggf.F jj_me2.F \
                   input_steer.F steer_new.F steer_pythia6.F \
                   ME2pythia.F ktclusdble.F getjet.F mlmmatch.F cas_merging.F alpsor.F \
                   cas_lheread.F casbran_new.F $(casc_source) $(tmdlib_source) 
cascade_SOURCES += histo.F get_ips.F                   
                   
#cascade_SOURCES += get_ips.cc histo.cc $(conv_source)  $(pythia8_source) updfgrid.cc  fjfort.cc fjcore.cc 
#cascade_SOURCES += $(conv_source)  $(pythia8_source) updfgrid.cc  fjfort.cc fjcore.cc 
#cascade_SOURCES += $(conv_source)  $(pythia8_source)  fjfort.cc fjcore.cc get_ips.cc histo.cc
cascade_SOURCES += $(conv_source)  $(pythia8_source)  fjfort.cc fjcore.cc 

cascade_LDADD =  ./libcascade3.la ../bases51/libcascadebases.la ../pythia6428/libcascadepythia.la \
                $(FLIBS) $(HEPMCLIB) $(HEPMC3LIB) $(RIVETLIB) $(TMDLIB) $(PDFLIB) $(GSL_LIB) $(PYTHIA8LIB)  ../misc/libcascademycern.la -lm 
                
	
