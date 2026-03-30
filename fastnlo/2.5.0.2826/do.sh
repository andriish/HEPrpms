#!/bin/bash
set -x
diff -Naur ORIG/ PATCHED/  > patch-fastnlo-0.txt
