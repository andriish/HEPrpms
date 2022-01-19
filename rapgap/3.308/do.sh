#!/bin/bash
rm -rf  patch-rapgap-0.txt

diff -Naur  ORIG/ PATCHED/ > patch-rapgap-0.txt
