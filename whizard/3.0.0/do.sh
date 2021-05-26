#!/bin/bash
set -x
rm -f patch-whizard-0.txt
rm -f patch-whizard-1.txt
diff -Naur ORIG/m4/     PATCHED/m4/  > patch-whizard-0.txt
diff -Naur ORIG/src/             PATCHED/src/  >           patch-whizard-1.txt
