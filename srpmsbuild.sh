#!/bin/bash
set -x
PACKAGE=$1
VERSION=$2
TOPDIR=$(pwd)
touch $TOPDIR/md5sums.txt.rej
export PATH=$PATH:/usr/bin:$TOPDIR
SPECTOOL=spectool
rm -rf $TOPDIR/$PACKAGE/$VERSION/rpmbuild/{SOURCES,SPECS,SRPMS} 
mkdir -p $TOPDIR/$PACKAGE/$VERSION/rpmbuild/{SOURCES,SPECS,RPMS,SRPMS} 
for a in $($SPECTOOL $TOPDIR/$PACKAGE/$VERSION/$PACKAGE.spec | tr -s ' '| cut -f 2 -d' ' | grep '://' ); do
aa=$(echo $a'#' |cut -f 1 -d '#')
bb=$(echo $a'#' |cut -f 2 -d '#')
if [ x$bb == x"" ];
then
bb=$(basename $aa)
fi
mkdir -p $TOPDIR/$PACKAGE/$VERSION/rpmbuild/SOURCES/
wget --no-check-certificate  --no-cache $aa -O $TOPDIR/$PACKAGE/$VERSION/rpmbuild/SOURCES/$bb
s=$(md5sum $PACKAGE/$VERSION/rpmbuild/SOURCES/$(basename $bb))
if grep -Fxq "$s" $TOPDIR/md5sums.txt
then
echo "MD5 sum->"$s"<-     Found in "$TOPDIR"/md5sums.txt"
else
echo "MD5 sum->"$s"<- NOT found in "$TOPDIR"/md5sums.txt"
echo $s >> $TOPDIR/md5sums.txt.rej
exit
fi

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
