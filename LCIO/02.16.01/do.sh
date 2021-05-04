#!/bin/bash
set -x
diff -Naur ORIG/ PATCHED/  > patch-LCIO-0.txt
