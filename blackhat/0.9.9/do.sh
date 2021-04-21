#!/bin/bash
set -x
#cp *.* ~/rpmbuild/SOURCES
diff -Naur  ORIG/src/cut_Darren.h    PATCHED/src/cut_Darren.h >           patch-blackhat-0.txt
diff -Naur  ORIG/src/cached_OLHA.cpp PATCHED/src/cached_OLHA.cpp >>       patch-blackhat-0.txt

diff -Naur  ORIG/blackhat-config.in  PATCHED/blackhat-config.in  >        patch-blackhat-1.txt

diff -Naur  ORIG/Makefile.am               PATCHED/Makefile.am  >               patch-blackhat-2.txt
diff -Naur  ORIG/utils/Makefile.am    PATCHED/utils/Makefile.am  >>             patch-blackhat-2.txt
diff -Naur  ORIG/src/eval_param.h     PATCHED/src/eval_param.h >>               patch-blackhat-2.txt
diff -Naur  ORIG/src/ME2_from_file.h  PATCHED/src/ME2_from_file.h >>            patch-blackhat-2.txt
diff -Naur  ORIG/src/assembly.h       PATCHED/src/assembly.h >>                 patch-blackhat-2.txt
diff -Naur  ORIG/src/from_file.h      PATCHED/src/from_file.h >>                patch-blackhat-2.txt
diff -Naur  ORIG/configure.ac         PATCHED/configure.ac  >>             patch-blackhat-2.txt
diff -Naur  ORIG/dataInstall.in       PATCHED/dataInstall.in  >>           patch-blackhat-2.txt

diff -Naur  ORIG/src/tree2.cc  PATCHED/src/tree2.cc >               patch-blackhat-3.txt
diff -Naur  ORIG/src/tree3.cc  PATCHED/src/tree3.cc >>              patch-blackhat-3.txt

