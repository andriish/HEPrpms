#!/bin/bash
rm -rf patch-PTL-0.txt
diff -Naur  ORIG/ PATCHED/ > patch-PTL-0.txt
