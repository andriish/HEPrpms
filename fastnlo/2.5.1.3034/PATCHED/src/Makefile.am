#                                               -*- Automake -*-
# -----------------------------------------------------------------------------
#
# K. Rabbertz, 06.10.2013
#
# fastNLO_toolkit
#
# -----------------------------------------------------------------------------
# Use GNU standards
AUTOMAKE_OPTIONS = gnu

#
# Targets
#
# The C++ fastnlo toolkit utility and example programs
#
if HAVE_LHAPDF
   bin_PROGRAMS = fnlo-tk-cppread fnlo-tk-example fnlo-tk-merge fnlo-tk-merge2 fnlo-tk-append fnlo-tk-modify fnlo-tk-cat
   bin_SCRIPTS  = fnlo-tk-config fnlo-py-print.py
# YODA features in fnlo-tk-statunc & fnlo-tk-yodaout are now conditionally compiled
#if HAVE_YODA
   bin_PROGRAMS += fnlo-tk-statunc
   bin_PROGRAMS += fnlo-tk-yodaout
#endif
#if HAVE_ROOT
   bin_PROGRAMS += fnlo-tk-rootout
#endif
endif

#
# Sources
#
# All source files which go into the targets (and have to be distributed)
# Scripts
EXTRA_DIST = fnlo-tk-config.in fnlo-py-print.py
# Programs to be compiled
fnlo_tk_cppread_SOURCES = fnlo-tk-cppread.cc
fnlo_tk_example_SOURCES = fnlo-tk-example.cc
fnlo_tk_yodaout_SOURCES = fnlo-tk-yodaout.cc
fnlo_tk_statunc_SOURCES = fnlo-tk-statunc.cc
fnlo_tk_rootout_SOURCES = fnlo-tk-rootout.cc
fnlo_tk_merge_SOURCES   = fnlo-tk-merge.cc
fnlo_tk_merge2_SOURCES  = fnlo-tk-merge2.cc
fnlo_tk_append_SOURCES  = fnlo-tk-append.cc
fnlo_tk_modify_SOURCES  = fnlo-tk-modify.cc
fnlo_tk_cat_SOURCES     = fnlo-tk-cat.cc

#
# Preprocessor flags
#
# All targets
AM_CPPFLAGS =
my_include_dirs = -I${srcdir}/../fastnlotoolkit/include -I${top_builddir}/fastnlotoolkit/include -I${top_srcdir}/fastnlotoolkit/include

if HAVE_LHAPDF
   my_include_dirs += -I${LHAPDFINCLUDEPATH}
endif

# Individual targets
fnlo_tk_cppread_CPPFLAGS = $(my_include_dirs) -I${includedir} $(AM_CPPFLAGS) $(LHAPDFCPPFLAGS)
fnlo_tk_example_CPPFLAGS = $(my_include_dirs) -I${includedir} $(AM_CPPFLAGS) $(ROOTCPPFLAGS)
fnlo_tk_yodaout_CPPFLAGS = $(my_include_dirs) -I${includedir} $(AM_CPPFLAGS) $(YODACPPFLAGS)
fnlo_tk_statunc_CPPFLAGS = $(my_include_dirs) -I${includedir} $(AM_CPPFLAGS) $(YODACPPFLAGS)
fnlo_tk_rootout_CPPFLAGS = $(my_include_dirs) -I${includedir} $(AM_CPPFLAGS) $(ROOTCPPFLAGS)
fnlo_tk_merge_CPPFLAGS   = $(my_include_dirs) -I${includedir} $(AM_CPPFLAGS)
fnlo_tk_merge2_CPPFLAGS  = $(my_include_dirs) -I${includedir} $(AM_CPPFLAGS)
fnlo_tk_append_CPPFLAGS  = $(my_include_dirs) -I${includedir} $(AM_CPPFLAGS)
fnlo_tk_modify_CPPFLAGS  = $(my_include_dirs) -I${includedir} $(AM_CPPFLAGS)
fnlo_tk_cat_CPPFLAGS     = $(my_include_dirs) -I${includedir} $(AM_CPPFLAGS)

#
# C++ compiler flags
#
# Recall: -std=c++17 is requested by default through configure.ac macro AX_CXX_COMPILE_STDCXX_17
#         and works with g++, clang etc.!
#         -std=gnu++17 is restricted to g++ only and not the best option
# All targets
AM_CXXFLAGS = -g -O2 -Wall -Wdeprecated-declarations
# Debug flags
# AM_CXXFLAGS = -g -ftrapping-math -fsignaling-nans -O0 -Wall
fnlo_tk_cppread_CXXFLAGS = $(AM_CXXFLAGS) $(HOPPETCXXFLAGS)
fnlo_tk_example_CXXFLAGS = $(AM_CXXFLAGS) $(ROOTCXXFLAGS)
fnlo_tk_yodaout_CXXFLAGS = $(AM_CXXFLAGS)
fnlo_tk_statunc_CXXFLAGS = $(AM_CXXFLAGS)
fnlo_tk_rootout_CXXFLAGS = $(AM_CXXFLAGS) $(ROOTCXXFLAGS)
fnlo_tk_merge_CXXFLAGS   = $(AM_CXXFLAGS)
fnlo_tk_merge2_CXXFLAGS  = $(AM_CXXFLAGS)
fnlo_tk_append_CXXFLAGS  = $(AM_CXXFLAGS)
fnlo_tk_modify_CXXFLAGS  = $(AM_CXXFLAGS)
fnlo_tk_cat_CXXFLAGS     = $(AM_CXXFLAGS)

#
# Linker flags
#
# Require lhapdf, use lhapdf-config if possible
# All targets
AM_LDFLAGS = -L${pkglibdir} -lfastnlotoolkit
if HAVE_HOPPET
   AM_LDFLAGS += -lgfortran
endif
my_lib_dirs = -L${srcdir}/../fastnlotoolkit -lfastnlotoolkit -L${top_builddir}/fastnlotoolkit -lfastnlotoolkit -L${top_srcdir}/fastnlotoolkit -lfastnlotoolkit
# Individual targets
fnlo_tk_cppread_LDFLAGS = $(my_lib_dirs) $(AM_LDFLAGS) $(LHAPDFLDFLAGS) $(HOPPETLDFLAGS)
fnlo_tk_example_LDFLAGS = $(my_lib_dirs) $(AM_LDFLAGS) $(LHAPDFLDFLAGS) $(ROOTLDFLAGS)
fnlo_tk_yodaout_LDFLAGS = $(my_lib_dirs) $(AM_LDFLAGS) $(LHAPDFLDFLAGS) $(YODALDFLAGS)
fnlo_tk_statunc_LDFLAGS = $(my_lib_dirs) $(AM_LDFLAGS) $(LHAPDFLDFLAGS) $(YODALDFLAGS)
fnlo_tk_rootout_LDFLAGS = $(my_lib_dirs) $(AM_LDFLAGS) $(LHAPDFLDFLAGS) $(ROOTLDFLAGS)
fnlo_tk_merge_LDFLAGS   = $(my_lib_dirs) $(AM_LDFLAGS) $(LHAPDFLDFLAGS) $(ROOTLDFLAGS)
fnlo_tk_merge2_LDFLAGS  = $(my_lib_dirs) $(AM_LDFLAGS) $(LHAPDFLDFLAGS) $(ROOTLDFLAGS)
fnlo_tk_append_LDFLAGS  = $(my_lib_dirs) $(AM_LDFLAGS) $(LHAPDFLDFLAGS) $(ROOTLDFLAGS)
fnlo_tk_modify_LDFLAGS  = $(my_lib_dirs) $(AM_LDFLAGS) $(LHAPDFLDFLAGS) $(ROOTLDFLAGS)
fnlo_tk_cat_LDFLAGS     = $(my_lib_dirs) $(AM_LDFLAGS) $(LHAPDFLDFLAGS) $(ROOTLDFLAGS)

# Files that are required to build the project but that can be
# recreated IF the necessary autotools etc. are available by:
# "autoreconf -i; ./configure"
# Use "make maintainer-clean" (on a build machine) before svn commits,
# but not for distribution
MAINTAINERCLEANFILES = Makefile.in
