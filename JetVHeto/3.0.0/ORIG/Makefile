
# edit the following two lines if lhapdf-config and hoppet-config are
# not in your path
LHAPDF_CONFIG="lhapdf-config"
HOPPET_CONFIG="hoppet-config"

# location where modules will be stored
MODULEPATH = $(PWD)/modules

# default fortran compiler
FC = gfortran
# store modules
FFLAGS = -O2 -fPIC 
# The option that causes modules to be stored in the $(MODULEPATH) directory
# depends on the compiler. Here it's provided for gfortran and ifort.  
ifeq ("$(FC)","gfortran")	
INCLUDE= -J$(MODULEPATH) -I$(MODULEPATH) `$(HOPPET_CONFIG) --fflags`
else 
ifeq ("$(FC)","ifort")
INCLUDE= -module $(MODULEPATH) -I$(MODULEPATH) `$(HOPPET_CONFIG) --fflags`
else
INCLUDE= -I. `$(HOPPET_CONFIG) --fflags`
endif
endif

LIBS = `$(HOPPET_CONFIG) --ldflags` `$(LHAPDF_CONFIG) --ldflags`

CHAPLIN=${HOME}/lib
#CHAPLIN=${HOME}/software/chaplin-1.2-install/lib 
LDFLAGS = -L$(CHAPLIN) -lchaplin

# select between smallR version of the code and the svn one
SOURCEDIR  = $(PWD)/src

VPATH      = $(SOURCEDIR):$(PWD)/obj

ALLPROG = jetvheto

ALLPROGOBJ = jetvheto.o

SRCS =	coefficient_functions.f90 ew_parameters.f90 emsn_tools.f90 \
	expansion.f90 io_utils.f90 lcl_dec.f90 matching.f90 \
	opts_cmdline_cardfile.f90 pdfs_tools.f90 rad_tools.f90 reader.f90 \
	resummation.f90 banner.f90 mass_corr.f90 interpolation.f90

POSTSRCS =	

OBJS =	coefficient_functions.o ew_parameters.o emsn_tools.o expansion.o \
	io_utils.o lcl_dec.o matching.o opts_cmdline_cardfile.o pdfs_tools.o \
	rad_tools.o reader.o resummation.o banner.o mass_corr.o interpolation.o

#TARFILES = JetZHeto/JetZHeto.tar.gz JetZHeto
#EXCLUDE = JetZHeto/.exclude-files



ALLOBJS = $(ALLPROGOBJ) $(OBJS) 

all: $(ALLPROG)

ALL:  $(ALLPROG)

jetvheto:  $(ALLOBJS)
	$(FC)  -o $@ $(patsubst %,obj/%,$(ALLOBJS)) $(LIBS) $(LDFLAGS)

check: $(ALLOBJS) jetvheto  
	scripts/run-checks.pl 

libclean:
	rm -f   obj/*.o  

clean:
	rm -f   obj/*.o  modules/*.mod *.d

distclean: clean
	rm -f  $(ALLPROG)

dist: src/banner.f90
	scripts/tarit.sh

.SUFFIXES: $(SUFFIXES) .f90

%.o: %.f90 
	$(FC) $(FFLAGS) $(INCLUDE) -c -o obj/$@ $<

%.o: %.f
	$(FC) $(FFLAGS) $(INCLUDE) -c -o obj/$@ $<

# a couple of lines to help with the combination of automatic
# generation of banner.f90 and the multi-directory build.
src/banner.f90: README scripts/makebanner.pl
	scripts/makebanner.pl $< $@
banner.o: src/banner.f90 
	$(FC) $(FFLAGS) $(INCLUDE) -c -o obj/$@ $<

coefficient_functions.o: 
ew_parameters.o: 
emsn_tools.o: rad_tools.o
expansion.o: emsn_tools.o pdfs_tools.o rad_tools.o
io_utils.o: 
jetvheto.o: ew_parameters.o expansion.o io_utils.o matching.o \
	opts_cmdline_cardfile.o pdfs_tools.o rad_tools.o reader.o \
	resummation.o banner.o mass_corr.o interpolation.o
matching.o: pdfs_tools.o rad_tools.o
opts_cmdline_cardfile.o: io_utils.o reader.o
pdfs_tools.o: coefficient_functions.o rad_tools.o ew_parameters.o
rad_tools.o: ew_parameters.o mass_corr.o
reader.o: io_utils.o
resummation.o: emsn_tools.o pdfs_tools.o rad_tools.o
mass_corr.o: ew_parameters.o
interpolation.o:
