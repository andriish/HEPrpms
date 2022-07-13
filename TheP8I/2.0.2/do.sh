#!/bin/bash
set -x
rm -rf patch-TheP8I-0.txt
diff -Naur ORIG/ PATCHED/  > patch-TheP8I-0.txt
