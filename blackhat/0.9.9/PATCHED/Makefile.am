ACLOCAL_AMFLAGS=-I m4

SUBDIRS =   \
	src/rational_eval \
	src/ratext src/cut src/cut_eval src/Interface \
	src/trees_eval \
	src  examples  \
	 datafiles  datafiles/parents datafiles/cut datafiles/rat datafiles/assembly datafiles/trees 
#	src/shared_trees my_programs

if ! PUBLIC
SUBDIRS += FeynDiagram
endif

if SHERPAPLUGIN
SUBDIRS += SherpaPlugin
endif

if PYTHONINTERFACE
SUBDIRS += Python
endif

# needs to be build last
SUBDIRS += utils
#if PUBLIC
#SUBDIRS += test
#endif

bin_SCRIPTS=blackhat-config dataInstall

slow:	
	make -C./src/trees/ slow
	make -C./src/cut/ slow
	make -C./src/rational/ slow



# if we have subversion, we compute the version at each call of install. 
# In order for this information to be useful, one should only use installed
# libraries in production runs. If there is no svnversion, then most probably
# the source comes from a tar-ball so the information on which version it
# came from has been copied in the share directory. This information has been 
# put there as the tar ball was created (we assume that the tar-ball will be
# created from a svn working copy).  

install-data-local:
	mkdir -p $(DESTDIR)/$(pkgdatadir)
#if HAS_SVNVERSION	
#	if ! test `svnversion  $(srcdir)` = "exported" ; then\
#		svnversion $(srcdir) > $(pkgdatadir)/svnversion ;\
#		svn info $(srcdir) | grep URL | grep -o "privatesvn/.*" | cut -d / -f2- > $(pkgdatadir)/svnurl ;\
#	fi
#endif
#	if test -f $(srcdir)/share/svnversion_dist ; then cp $(srcdir)/share/svnversion_dist $(pkgdatadir)/svnversion_dist; fi
#	if test -f $(srcdir)/share/svnurl_dist ; then cp $(srcdir)/share/svnurl_dist $(pkgdatadir)/svnurl_dist ; fi
	
dist-hook:
	mkdir -p $(DESTDIR)/$(distdir)/share
#	svnversion $(srcdir) > $(distdir)/share/svnversion_dist
#	svn info $(srcdir) | grep URL | grep -o "privatesvn/.*" | cut -d / -f2- > $(distdir)/share/svnurl_dist
#	for d in Z2 Wm3 3j Wm1 log Y3 Z1 Wm2 Y2 Wm data Y1 Z Z3 2j ; do rm -rf $(distdir)/test/PStest/$$d/.svn ; done 
#	rm -rf $(distdir)/test/PStest/.svn

	
if ! PUBLIC
install-data-hook:
else
install-data-hook:
	sh dataInstall $(abs_srcdir)/BHdata.tar.gz $(DESTDIR)
	touch $(DESTDIR)/$(prefix)/share/blackhat/svnversion_dist
	touch $(DESTDIR)/$(prefix)/share/blackhat/svnurl_dist
	touch $(DESTDIR)/$(prefix)/share/blackhat/svnversion
	touch $(DESTDIR)/$(prefix)/share/blackhat/svnurl

uninstall-hook:
	rm -rf $(prefix)/share/blackhat/datafiles
	rm -rf $(pkgdatadir)/svnversion_dist
	rm -rf $(pkgdatadir)/svnurl_dist
	rm -rf $(pkgdatadir)/svnversion
	rm -rf $(pkgdatadir)/svnurl
endif	
	
#if PYTHONINTERFACE
#install-exec-hook:
#	ln -fs ${prefix}/lib/libPythonInterface.so ${prefix}/lib/_BH.so
#	cp $(builddir)/Python/BH.py ${prefix}/lib/
#endif
	
if PUBLIC
EXTRA_DIST = BHdata.tar.gz
#EXTRA_DIST += datafiles


endif	
	
