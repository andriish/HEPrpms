#!/bin/bash
set -x
diff -Naur ORIG/ PATCHED/  > patch-recola2-0.txt
