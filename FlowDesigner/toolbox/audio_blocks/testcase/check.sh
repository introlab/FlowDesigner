#!/bin/sh

SETCOLOR_SUCCESS="echo -en \\033[1;32m"
SETCOLOR_FAILURE="echo -en \\033[1;31m"
SETCOLOR_WARNING="echo -en \\033[1;33m"
SETCOLOR_NORMAL="echo -en \\033[0;39m"

echo_success() {
echo -n "[  "
$SETCOLOR_SUCCESS
echo -n "OK"
$SETCOLOR_NORMAL
echo "  ]"
}

echo_failure() {
echo -n "[  "
$SETCOLOR_FAILURE
echo -n "FAILED"
$SETCOLOR_NORMAL
echo "  ]"
}

for file in *.n non-existant.n
	do
	echo -ne "$file  \t"
	if test -f $file.out
		then
		if batchflow $file | grep -qf $file.out
			then
			echo_success
		else
			echo -ne "$file  \t"
			echo_failure
			echo
		fi
	else
		batchflow $file > /dev/null 2>&1
		if test $? -lt 2
			then
			echo_success
		else
			echo_failure
			echo
		fi
	fi
done
