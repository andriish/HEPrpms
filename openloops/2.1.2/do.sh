#!/bin/bash
set -x
diff -Naur ORIG/ PATCHED/  > patch-openloops-0.txt
