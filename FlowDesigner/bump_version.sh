#!/bin/sh
ARG=$1
MAJOR=`echo $1 | awk -F. '{print $1}'`
MINOR=`echo $1 | awk -F. '{print $2}'`
MICRO=`echo $1 | awk -F. '{print $3}'`
#echo $MAJOR
#echo $MINOR
#echo $MICRO
#exit
for i in configure.in */configure.in
do
cat $i | sed -e "s/FLOWDESIGNER_MICRO_VERSION=.*\$/FLOWDESIGNER_MICRO_VERSION=$MICRO/" \
-e "s/FLOWDESIGNER_MAJOR_VERSION=.*\$/FLOWDESIGNER_MAJOR_VERSION=$MAJOR/" \
-e "s/FLOWDESIGNER_MINOR_VERSION=.*\$/FLOWDESIGNER_MINOR_VERSION=$MINOR/" > tmp
\mv tmp $i
done
