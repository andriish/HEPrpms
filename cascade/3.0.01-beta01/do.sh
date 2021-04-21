#!/bin/bash
set -x
rm -f  patch-cascade-0.txt
touch  patch-cascade-0.txt
diff -Naur  ORIG/ PATCHED/ >>      patch-cascade-0.txt
