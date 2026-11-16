#!/bin/bash
set -x
diff -Naur ORIG/ PATCHED/  > patch-Rivet-0.txt
