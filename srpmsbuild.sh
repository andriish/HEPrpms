#!/bin/bash
set -x
PACKAGE=$1
VERSION=$2

TOPDIR=$(pwd)
export PATH=$PATH:/usr/bin:$TOPDIR
SPECTOOL=spectool
rm -rf $TOPDIR/$PACKAGE/$VERSION/rpmbuild/{SOURCES,SPECS,SRPMS} 
mkdir -p $TOPDIR/$PACKAGE/$VERSION/rpmbuild/{SOURCES,SPECS,RPMS,SRPMS} 
for a in $($SPECTOOL $TOPDIR/$PACKAGE/$VERSION/$PACKAGE.spec | tr -s ' '| cut -f 2 -d' ' | grep '://' ); do
wget --no-check-certificate  --no-cache $a -P $TOPDIR/$PACKAGE/$VERSION/rpmbuild/SOURCES
done 
for a in $($SPECTOOL $TOPDIR/$PACKAGE/$VERSION/$PACKAGE.spec | tr -s ' '| cut -f 2 -d' ' | grep -v '://' ); do
cp $TOPDIR/$PACKAGE/$VERSION/$(basename $a) $TOPDIR/$PACKAGE/$VERSION/rpmbuild/SOURCES
done 


cd $TOPDIR/$PACKAGE/$VERSION
if [ -f do.sh ]; 
then
sh do.sh
fi
cd $TOPDIR
cp $TOPDIR/$PACKAGE/$VERSION/*$PACKAGE*  $TOPDIR/$PACKAGE/$VERSION/rpmbuild/SOURCES
rpmbuild -bs --define='%_topdir '$TOPDIR/$PACKAGE/$VERSION'/rpmbuild' $TOPDIR/$PACKAGE/$VERSION/$PACKAGE.spec
if [ x$3 == x"--build" ]; 
then
rpmbuild --define='%_topdir '$TOPDIR/$PACKAGE/$VERSION'/rpmbuild' --rebuild $TOPDIR/$PACKAGE/$VERSION/rpmbuild/SRPMS/$PACKAGE*src.rpm   --nodeps
fi
