#!/bin/bash
rm -rf patch-Delphes-0.txt
diff -Naur  ORIG/  PATCHED/ > patch-Delphes-0.txt
