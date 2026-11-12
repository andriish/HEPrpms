#!/bin/bash
set -x
diff -Naur ORIG/ PATCHED/  > patch-fastnlo-0.txt

# Only for EL8/EL9 (autoconf 2.69): applied on top of patch-fastnlo-0.txt
diff -Naur ORIG_EL9/ PATCHED_EL9/ > patch-fastnlo-1.txt
