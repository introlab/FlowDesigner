#!/bin/sh

for i in `cd $2; grep NODE_INFO *.cc | cut -f 1 -d :`
do
file=`echo $i | cut -f 1 -d .`.def
perl $1 $2/$i > "$3/$file"
done
