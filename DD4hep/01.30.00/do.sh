#!/bin/bash
set -x
diff -Naur ORIG/ PATCHED/  > patch-DD4hep-0.txt
