#!/bin/bash
set -x
rm -rf patch-fjcontrib-0.txt
diff -Naur  ORIG PATCHED > patch-onnxruntime-0.txt

