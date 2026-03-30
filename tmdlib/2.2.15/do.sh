#!/bin/bash
set -x

rm -f  patch-tmdlib-0.txt
touch  patch-tmdlib-0.txt
diff -Naur  ORIG/ PATCHED/ >      patch-tmdlib-0.txt
