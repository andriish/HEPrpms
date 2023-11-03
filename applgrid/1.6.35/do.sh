#!/bin/bash
rm -rf patch-applgrid-0.txt
diff -Naur  ORIG/ PATCHED/ > patch-applgrid-0.txt
