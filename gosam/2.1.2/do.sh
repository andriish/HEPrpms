#!/bin/bash
rm -rf patch-gosam-0.txt
diff -Naur  ORIG/ PATCHED/ > patch-gosam-0.txt
