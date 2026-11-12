#!/bin/bash
rm -rf  patch-cascade-0.txt

diff -Naur  ORIG/ PATCHED/ > patch-cascade-0.txt
