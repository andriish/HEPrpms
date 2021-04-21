#!/bin/bash
wget https://github.com/andriish/HEPsources/raw/master/cernlib-2006-35.el6.src.rpm
rpm -ivh cernlib-2006-35.el6.src.rpm --define='%_topdir '$(pwd)'/rpmbuild'
