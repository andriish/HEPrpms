#!/bin/bash
rm -rf  patch-ariadne-0.txt

diff -Naur  ORIG/ PATCHED/ > patch-ariadne-0.txt
