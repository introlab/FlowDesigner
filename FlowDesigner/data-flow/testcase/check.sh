#!/bin/sh

for file in *.n non-existant.n
	do
	if test -f $file.out
		then
		if batchflow $file | grep -qf $file.out
			then
			echo $file passed
		else
			echo "** $file failed **"
			echo
		fi
	else
		batchflow $file > /dev/null 2>&1
		if test $? -lt 2
			then
			echo $file passed
		else
			echo "** $file failed **"
			echo
		fi
	fi
done
