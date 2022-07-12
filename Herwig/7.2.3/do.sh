#!/bin/bash
set -x
rm -rf patch-Herwig-0.txt
diff -Naur ORIG/ PATCHED/  > patch-Herwig-0.txt
