#!/bin/bash
set -x
rm -rf patch-SHERPA-MC-0.txt
diff -Naur ORIG/ PATCHED/  > patch-SHERPA-MC-0.txt
