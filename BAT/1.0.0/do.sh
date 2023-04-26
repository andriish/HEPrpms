#!/bin/bash
set -x
rm -rf patch-BAT-0.txt
diff -Naur ORIG/ PATCHED/  > patch-BAT-0.txt
