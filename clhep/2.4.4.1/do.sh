#!/bin/bash
set -x
diff -Naur ORIG/ PATCHED/  > patch-clhep-0.txt
