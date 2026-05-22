#!/bin/bash
set -x
diff -Naur ORIG/ PATCHED/  > patch-YODA-0.txt
