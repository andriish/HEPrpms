##########################################################################
#
#   File:         Makefile.am
#
#                 Copyright (C) M.Sutton (sutt@cern.ch) 
#
#   Description:  generate the Makefile for the appl_grid project
#                 
#
#   Created:    M.Sutton (sutt@hep.ucl.ac.uk) Fri  5 Nov 2010 00:05:29 GMT
#   
#   $Id: makefile 20 2008-05-05 01:55:42Z sutt $                
#                   
#########################################################################


if USE_ROOT
  ROOTCFLAGS = $(shell root-config --cflags)
  ROOTLIBS   = $(shell root-config --nonew --libs)
  ROOTGLIBS  = $(shell root-config --nonew --glibs)
  ROOTARCH   = $(shell root-config --ldflags)
  ROOTCINT=rootcint
else
  ROOTCFLAGS = 
  ROOTLIBS   = 
  ROOTGLIBS  = 
  ROOTARCH   = 
  ROOTCINT   =
endif


# hoppet flags
if USE_HOPPET 
  HOPPETDIR    = $(shell hoppet-config --prefix)
  HOPPETCFLAGS = $(shell hoppet-config --cxxflags)
  HOPPETLIBS   = $(shell hoppet-config --libs) 
else
  HOPPETDIR    = 
  HOPPETCFLAGS = 
  HOPPETLIBS   = 
endif


# fotran RTL

FRTLLIB =

if USE_FRTLSO
    FRTLIB   = $(shell gfortran -print-file-name=libgfortran.so)
    FRTLLIB  = -L$(subst /libgfortran.so, ,$(FRTLIB) ) -lgfortran 
else
    FRTLIB   = $(shell gfortran -print-file-name=libgfortran.a)
    FRTLLIB  = -L$(subst /libgfortran.a, ,$(FRTLIB) ) -lgfortran 
endif


# LHAPDF flags 

if USE_LHAPDF
  # in preparation for including the lhapdf version 6 interface
  LHAPDFCFLAGS = $(shell lhapdf-config --cxxflags)
else
  LHAPDFCFLAGS = 
endif

AM_CXXFLAGS = $(ROOTARCH) -O3 -DDATADIR=\"@datadir@/$(PACKAGE)\" -Wall -Wextra -fPIC  -I. -I.. -I$(srcdir) $(LHAPDFCXXFLAGS) $(ROOTCFLAGS)  $(HOPPETCFLAGS) -fPIC

AM_LDFLAGS = $(ROOTARCH) -O3  $(ROOTLIBS) $(FCLIBS) $(HOPPETLIBS) $(FRTLLIB)  


bin_PROGRAMS = applgrid-combine applgrid-convert

applgrid_combine_SOURCES = combine.cxx
applgrid_combine_LDADD   = libAPPLgrid.la
applgrid_combine_CXXFLAGS =  $(AM_CXXFLAGS) 
applgrid_combine_LDFLAGS = $(ROOTARCH) -L. -lAPPLgrid $(ROOTLIBS) $(HOPPETLIBS) $(FRTLLIB)

applgrid_convert_SOURCES = convert.cxx
applgrid_convert_LDADD   = libAPPLgrid.la
applgrid_convert_CXXFLAGS =  $(AM_CXXFLAGS) 
applgrid_convert_LDFLAGS = $(ROOTARCH) -L. -lAPPLgrid $(ROOTLIBS) $(HOPPETLIBS) $(FRTLLIB)


lib_LTLIBRARIES= libAPPLgrid.la libfAPPLgrid.la 
libAPPLgrid_la_SOURCES = \
	appl_grid.cxx		appl_igrid.cxx       fastnlo.cxx \
	appl_timer.cxx          appl_pdf.cxx         \
	appl_file.cxx                                \
	file_index.cxx                               \
	histogram.cxx                                \
	nlojet_pdf.cxx		nlojetpp_pdf.cxx     \
	mcfmw_pdf.cxx		mcfmwjet_pdf.cxx     \
	mcfmwc_pdf.cxx                               \
	mcfmz_pdf.cxx mcfmzjet_pdf.cxx               \
	mcfmQQ_pdf.cxx                               \
	jetrad_pdf.cxx		dis_pdf.cxx          \
	generic_pdf.cxx         basic_pdf.cxx        \
	combination.cxx         lumi_pdf.cxx         \
	SparseMatrix3d.cxx	hoppet_init.cxx       \
	TFileString.cxx        \
	TFileVector.cxx	\
	integral.cxx \
	*.h

if USE_ROOT
  libAPPLgrid_la_SOURCES += TFileStringDict.cxx TFileVectorDict.cxx 
endif

libfAPPLgrid_la_SOURCES = fappl_grid.cxx fappl_fitter.cxx

library_includedir=$(includedir)/appl_grid

library_include_HEADERS=../appl_grid/*.h


# GSLLIBS=$(shell gsl-config --libs)
# WDIR = $(shell pwd)


# if WITHCLING
### ROOT-6: ###

# rootmapdir = $(libdir)
# rootmap_DATA = .libs/*.pcm

# endif


AM_FCFLAGS = -c
AM_FCLIBS  = -lg2c
AM_SOFLAGS = -shared
CINT       = rootcint

clean-local:
	rm -rf *.o *.lo *Dict*
.cxx.o : 
	$(CXX) $(AM_CXXFLAGS) -c $<
.c.o : 
	$(CC) $(AM_CFLAGS) -c $<

if USE_ROOT
%Dict.cxx : %.h %.cxx
	$(CINT) -f $@ -c $< -I.. 
endif

#../appl_grid/$*LinkDef.h
