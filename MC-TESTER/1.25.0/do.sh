#!/bin/bash
rm -rf  patch-MC-TESTER-0.txt

diff -Naur  ORIG/ PATCHED/ > patch-MC-TESTER-0.txt
