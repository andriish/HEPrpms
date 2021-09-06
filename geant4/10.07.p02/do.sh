#!/bin/bash
set -x
diff -Naur ORIG/ PATCHED/  > patch-geant4-0.txt
