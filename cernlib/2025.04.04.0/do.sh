#!/bin/bash
set -x
diff -Naur ORIG/ PATCHED/  > patch-cernlib-0.txt
