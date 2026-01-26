#!/bin/bash
set -x
diff -Naur ORIG/ PATCHED/  > patch-cernlib-0.txt
diff -Naur ORIG1/ PATCHED1/  > patch-cernlib-1.txt
