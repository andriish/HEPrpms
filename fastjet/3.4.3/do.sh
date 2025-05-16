#!/bin/bash
set -x
diff -Naur ORIG/ PATCHED/  > patch-fastjet-0.txt
