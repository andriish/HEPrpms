#!/bin/bash
set -x
diff -Naur ORIG/ PATCHED/  > patch-PHOTOS-0.txt
