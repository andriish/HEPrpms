#!/bin/bash
set -x
diff -Naur ORIG/ PATCHED/  > patch-TAUOLA-0.txt
