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
cat $i | sed -e "s/FREESPEECH_MICRO_VERSION=.*\$/FREESPEECH_MICRO_VERSION=$MICRO/" \
-e "s/FREESPEECH_MAJOR_VERSION=.*\$/FREESPEECH_MAJOR_VERSION=$MAJOR/" \
-e "s/FREESPEECH_MINOR_VERSION=.*\$/FREESPEECH_MINOR_VERSION=$MINOR/" > tmp
\mv tmp $i
done
