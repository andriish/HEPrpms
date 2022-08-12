#!/bin/bash
set -x
diff -Naur ORIG/ PATCHED/  > patch-MCFM-0.txt
