#!/bin/bash
set -x
diff -Naur ORIG/ PATCHED/  > patch-SHERPA-MC-0.txt
