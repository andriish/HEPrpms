SUBDIRS = BLHAGeneric VBFNLO NJet GoSam OpenLoops MadGraph


pkglib_LTLIBRARIES =

##############

if HAVE_GOSAM
pkglib_LTLIBRARIES += HwMatchboxGoSam.la
endif

HwMatchboxGoSam_la_LDFLAGS = \
$(AM_LDFLAGS) -module -version-info 15:0:0

HwMatchboxGoSam_la_CPPFLAGS = $(AM_CPPFLAGS) \
-DHERWIG_BINDIR="\"$(bindir)\"" \
-DHERWIG_PKGDATADIR="\"$(pkgdatadir)\"" \
-DGOSAM_PREFIX="\"$(GOSAMPREFIX)\""

HwMatchboxGoSam_la_SOURCES = \
GoSam/GoSamAmplitude.cc


###############

if HAVE_VBFNLO
pkglib_LTLIBRARIES += HwMatchboxVBFNLO.la
endif

HwMatchboxVBFNLO_la_LDFLAGS = $(AM_LDFLAGS) -module -version-info 5:0:0
HwMatchboxVBFNLO_la_CPPFLAGS = $(AM_CPPFLAGS)
HwMatchboxVBFNLO_la_CPPFLAGS += -I$(VBFNLOINCLUDE)
HwMatchboxVBFNLO_la_CPPFLAGS += -DVBFNLOLIB=$(VBFNLOLIB)

HwMatchboxVBFNLO_la_SOURCES = \
VBFNLO/VBFNLOAmplitude.cc \
VBFNLO/VBFNLOPhasespace.cc


###############

if HAVE_OPENLOOPS
pkglib_LTLIBRARIES += HwMatchboxOpenLoops.la
endif

HwMatchboxOpenLoops_la_SOURCES = \
OpenLoops/OpenLoopsAmplitude.cc

HwMatchboxOpenLoops_la_LDFLAGS = $(AM_LDFLAGS) -module -version-info 15:0:0

HwMatchboxOpenLoops_la_CPPFLAGS = $(AM_CPPFLAGS) \
-DOPENLOOPSLIBS="\"$(OPENLOOPSLIBS)\"" \
-DOPENLOOPSPREFIX="\"$(OPENLOOPSPREFIX)\""

##############


if HAVE_NJET
pkglib_LTLIBRARIES += HwMatchboxNJet.la
endif

HwMatchboxNJet_la_LDFLAGS = $(AM_LDFLAGS) -module -version-info 15:0:0

HwMatchboxNJet_la_CPPFLAGS = $(AM_CPPFLAGS) -I$(NJETINCLUDEPATH) \
-DNJET_PREFIX="\"$(NJETPREFIX)\"" \
-DNJET_LIBS="\"$(NJETLIBPATH)\"" \
-DNJET_VERSION="$(NJET_VERSION)"

HwMatchboxNJet_la_SOURCES = \
NJet/NJetsAmplitude.cc



##############

if HAVE_MADGRAPH
pkglib_LTLIBRARIES += HwMatchboxMadGraph.la
endif

HwMatchboxMadGraph_la_LDFLAGS = $(AM_LDFLAGS) -module -version-info 6:0:0

HwMatchboxMadGraph_la_SOURCES = \
MadGraph/MadGraphAmplitude.cc

HwMatchboxMadGraph_la_CPPFLAGS = $(AM_CPPFLAGS) \
-DHERWIG_BINDIR="\"$(bindir)\"" \
-DHERWIG_INCLUDEDIR="\"$(includedir)\"" \
-DHERWIG_PKGDATADIR="\"$(pkgdatadir)\"" \
-DMADGRAPH_PREFIX="\"$(MADGRAPHPREFIX)\""







