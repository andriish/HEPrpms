#!/bin/bash
set -x
diff -Naur ORIG/ PATCHED/  > patch-qcdloop-0.txt
