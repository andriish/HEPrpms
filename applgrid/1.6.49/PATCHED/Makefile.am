AUTOMAKE_OPTIONS = foreign 
ACLOCAL  = aclocal
AUTOMAKE = automake

docdir   = ${datadir}/doc/${PACKAGE}
dist_doc_DATA   = README INSTALL COPYING

sharedir = ${datadir}/${PACKAGE}
dist_share_DATA = share/*.config 

if USE_ROOT
install-data-am:
	cp src/*.pcm $(DESTDIR)${libdir}
endif

clean-local:
	rm -rf autom4te.cache

SUBDIRS = src bin

## Clean out SVN files and root dictionaries
dist-hook:
	rm -rf `find $(distdir) -name ".svn"` `find $(distdir) -name \*Dict\*`
