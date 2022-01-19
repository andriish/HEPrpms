#!/bin/bash
set -x
rm -f patch-whizard-0.txt
diff -Naur ORIG/     PATCHED/  > patch-whizard-0.txt
