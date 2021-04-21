#!/bin/bash

#diff -Naur  m4_ORIG/  m4/  > patch-Herwig-0.txt
rm -rf patch-Herwig-0.txt
diff -Naur  PATCHED_ORIG/m4/  PATCHED/m4  >> patch-Herwig-0.txt
