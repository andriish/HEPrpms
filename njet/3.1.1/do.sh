#!/bin/bash
rm -rf patch-njet-0.txt
diff -Naur  ORIG/ PATCHED/ > patch-njet-0.txt
