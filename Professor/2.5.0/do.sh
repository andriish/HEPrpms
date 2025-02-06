#!/bin/bash
set -x
rm -f  patch-Professor-0.txt
diff -Naur ORIG/ PATCHED >      patch-Professor-0.txt

