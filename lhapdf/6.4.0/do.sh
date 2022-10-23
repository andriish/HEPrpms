#!/bin/bash
set -x
rm -rf patch-lhapdf-0.txt
diff -Naur ORIG/ PATCHED/  > patch-lhapdf-0.txt
