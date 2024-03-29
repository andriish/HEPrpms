# If you are using this Makefile standalone and fastjet-config is not
# in your path, edit this line to specify the full path
FASTJETCONFIG=fastjet-config
PREFIX=`$(FASTJETCONFIG) --prefix`
LIBDIR=$(PREFIX)/lib
CXX=g++
CXXFLAGS= -O3 -Wall -g
install_script = ${SHELL} ../utils/install-sh
check_script = ../utils/check.sh

# global contrib-wide Makefile include may override some of the above
# variables (leading "-" means don't give an error if you can't find
# the file)
-include ../.Makefile.inc

#------------------------------------------------------------------------
# things that are specific to this contrib
NAME=JetFFMoments
SRCS=JetFFMoments.cc
INSTALLED_HEADERS=JetFFMoments.hh
#------------------------------------------------------------------------

# add fastjet libraries
CXXFLAGS+= -fPIC   $(shell ${FASTJETCONFIG} --cxxflags)
LDFLAGS+=-lm $(shell ${FASTJETCONFIG} --libs)

OBJS  = ${SRCS:.cc=.o}

install_HEADER  = $(install_script) -c -m 644
install_LIB     = $(install_script) -c -m 755
install_DIR     = $(install_script) -d

.PHONY: clean install examples

all: lib$(NAME).a  lib$(NAME).so

lib$(NAME).a: $(OBJS) 
	ar cru lib$(NAME).a $(OBJS)
	ranlib lib$(NAME).a
lib$(NAME).so: $(OBJS) 
	$(CXX) -shared -o lib$(NAME).so $(OBJS)
examples: example

example: all example.o
	$(CXX) -o example example.o -L. -l$(NAME) $(LDFLAGS)

check: examples
	@$(check_script) example ../data/Pythia-Zp2jets-lhc-pileup-1ev.dat || exit 1
	@echo "All tests successful"

examples: example

clean:
	rm -f *~ *.o

distclean: clean
	rm -f lib$(NAME).a lib$(NAME).so example

install: all
	$(install_DIR) $(DESTDIR)$(PREFIX)/include/fastjet/contrib
	$(install_HEADER) JetFFMoments.hh $(DESTDIR)$(PREFIX)/include/fastjet/contrib
	$(install_DIR) $(DESTDIR)$(LIBDIR)
	$(install_LIB) lib$(NAME).a $(DESTDIR)$(LIBDIR)
	$(install_LIB) lib$(NAME).so $(DESTDIR)$(LIBDIR)

depend:
	makedepend -Y --   -- $(SRCS) example.cc
# DO NOT DELETE

JetFFMoments.o: JetFFMoments.hh
example.o: JetFFMoments.hh
