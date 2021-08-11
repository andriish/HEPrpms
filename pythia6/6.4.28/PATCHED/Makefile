#SRCS := $(wildcard py*.f) $(wildcard struct*.f) $(wildcard up*.f) ssmssm.f sugra.f visaje.f $(wildcard fh*.f) 
#pdfset.f
SRCS := $(wildcard py*.f)   ssmssm.f sugra.f visaje.f $(wildcard fh*.f) 
OBJS = ${SRCS:.f=.o}

SRCSD :=  $(wildcard up*.f) 
OBJSD = ${SRCSD:.f=.o}
# archive executable
AR = ar
# FC = g77
FC = gfortran

message:
	@echo 'You can make lib or clean'
	@echo 'Please do not clash with the names in the library'

clean:
	rm -rf libpythia6.a *.o

lib: libpythia6.a libpythia6.so  libpythia6_dummy.a libpythia6_dummy.so

# Make lib
libpythia6.a: ${OBJS}
	$(AR) rcv $@ $+


libpythia6_dummy.a: ${OBJSD}
	$(AR) rcv $@ $+


libpythia6.so: ${OBJS}
	$(LD) $(LDFLAGS) -shared  $+ -o libpythia6.so --no-as-needed -lgfortran

libpythia6_dummy.so: ${OBJSD}
	$(LD) $(LDFLAGS) -shared  $+ -o libpythia6_dummy.so --no-as-needed  -lgfortran


bigfile:
	cat ${SRCS} >> .tmp_file;
	mv .tmp_file pythia6.f
# DO NOT DELETE
