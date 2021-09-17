noinst_LTLIBRARIES  = libHwLooptoolsXFC.la libHwLooptoolsCFC.la 
noinst_LTLIBRARIES += libHwLooptools.la

EXTRA_DIST = \
include/cexternals.h \
include/clooptools.h \
include/defs.h \
include/externals.h \
include/fferr.h \
include/ff.h \
include/ffperm5.h \
include/ffwarn.h \
include/ftypes.h \
include/looptools.h \
include/lt.h \
include/ltnames.h \
include/perm.h \
include/types.h \
util/solve-LU.F \
util/solve-LU.h \
util/solve-Eigen.F

theXFCSOURCES = \
A/Aget.F     A/ffxa0.F   A/ffca0.F \
B/Bget.F     B/Bcoeff.F  B/BcoeffC.F B/BcoeffAD.F B/BcoeffFF.F \
B/ffxb0.F    B/ffcb0.F   B/ffxb1.F   B/ffcb1.F    B/ffxb2p.F   B/ffcb2p.F \
B/ffxdb0.F   B/ffcdb0.F  B/ffxdb1.F  B/ffdel2.F   B/ffcel2.F \
C/C0func.F   C/C0funcC.F C/Cget.F    C/ffxc0.F    C/ffcc0.F \
C/ffxc0i.F   C/ffxc0p.F  C/ffxc0p0.F C/ffcc0p.F   C/ffdxc0.F \
C/ffdel3.F   C/ffcel3.F  \
D/D0func.F   D/D0funcC.F D/Dget.F \
D/ffxd0.F    D/ffxd0h.F  D/ffxd0i.F  D/ffxd0p.F   D/ffxd0m0.F \
D/ffxd0tra.F D/ffxdbd.F  D/ffdcc0.F  D/ffdel4.F   D/ffd0c.F \
D/ffTn.F     D/ffT13.F   D/ffS2.F    D/ffS3n.F    D/ffRn.F \
E/E0func.F   E/Eget.F    E/Ecoeffa.F E/Ecoeffb.F \
E/ffxe0.F    E/ffdel5.F \
util/ini.F    util/auxCD.F  util/solve.F  util/Dump.F \
util/Li2.F    util/Li2omx.F util/ffinit.F util/ffxli2.F util/ffcli2.F util/ffxxyz.F \
util/ffcxyz.F util/ffcrr.F  util/ffcxr.F  util/fftran.F util/ffabcd.F \
util/ff2dl2.F util/ffcxs3.F util/ffcxs4.F util/ffdcxs.F util/ffbndc.F

## the following need -DCOMPLEXPARA
theCFCSOURCES = \
A/Aget.F   B/Bget.F B/BcoeffFF.F C/Cget.F D/Dget.F  \
E/E0func.F E/Eget.F E/Ecoeffa.F  E/Ecoeffb.F  \
util/solve.F util/Dump.F util/Li2.F util/Li2omx.F

commonCPPFLAGS = $(AM_CPPFLAGS) -I$(srcdir)/include

libHwLooptoolsXFC_la_SOURCES = $(theXFCSOURCES)
libHwLooptoolsXFC_la_CPPFLAGS = $(commonCPPFLAGS)
libHwLooptoolsXFC_la_FCFLAGS = $(AM_FCFLAGS)
libHwLooptoolsXFC_la_FFLAGS  = $(AM_FFLAGS)

libHwLooptoolsCFC_la_SOURCES = $(theCFCSOURCES)
libHwLooptoolsCFC_la_CPPFLAGS = $(commonCPPFLAGS) -DCOMPLEXPARA
libHwLooptoolsCFC_la_FCFLAGS = $(AM_FCFLAGS)
libHwLooptoolsCFC_la_FFLAGS  = $(AM_FFLAGS)

libHwLooptools_la_SOURCES = util/cache.c clooptools.cc
libHwLooptools_la_CXXFLAGS = $(AM_CXXFLAGS) -Wno-strict-aliasing
libHwLooptools_la_CPPFLAGS = $(commonCPPFLAGS) 
libHwLooptools_la_CFLAGS = $(AM_CFLAGS) -std=c99 
libHwLooptools_la_LIBADD = libHwLooptoolsXFC.la libHwLooptoolsCFC.la

EXTRA_PROGRAMS = tester
tester_SOURCES = tester.cc
tester_LDADD = libHwLooptools.la $(FCLIBS) $(THEPEGLDFLAGS) $(THEPEGLIB)
