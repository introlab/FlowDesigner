#!/bin/sh

for file in *.n non-existant.n
	do
	echo -n "$file... "
	if test -f $file.out
		then
		if batchflow $file | grep -qf $file.out
			then
			echo passed
		else
			echo "** $file failed **"
			echo
		fi
	else
		batchflow $file > /dev/null 2>&1
		if test $? -lt 2
			then
			echo passed
		else
			echo "**failed **"
			echo
		fi
	fi
done
