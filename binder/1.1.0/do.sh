#!/bin/bash
set -x
diff -Naur ORIG/ PATCHED/  > patch-binder-0.txt
