SRCS := $(wildcard py*.f) $(wildcard struct*.f) $(wildcard up*.f) ssmssm.f sugra.f visaje.f $(wildcard fh*.f) pdfset.f
OBJS = ${SRCS:.f=.o}
# archive executable
AR = ar
# FC = g77
FC = gfortran
# Fortran compiler
FFLAGS = -g2 -O2 -ffortran-bounds-check
FFLAGS = -g -O2 -fbounds-check
FFLAGS = -g -fbounds-check

message:
	@echo 'You can make lib or clean'
	@echo 'Please do not clash with the names in the library'

clean:
	rm -rf libpythia.a *.o

lib: libpythia.a

# Make lib
libpythia.a: ${OBJS}
	$(AR) rcv $@ $+

bigfile:
	cat ${SRCS} >> .tmp_file;
	mv .tmp_file pythia6.f
# DO NOT DELETE
