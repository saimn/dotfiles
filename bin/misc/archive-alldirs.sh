#!/bin/sh
# make a tar.gz archive for each sub-directory

dirlist=`ls $1`

for f in $dirlist
do
    if [ -d $f ]
    then
       echo ":: $f -> $f.tar.gz"
       tar -czvf $f.tar.gz $f
    fi
done

exit
