#!/bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

PKG_NAME="Gnome Balsa"

(test -f $srcdir/configure.in \
  && test -d $srcdir/data-flow) || {
    echo -n "**Error**: Directory "\`$srcdir\'" does not look like the"
    echo " top-level FreeSpeech directory"

    exit 1
}

. $srcdir/macros/autogen.sh
