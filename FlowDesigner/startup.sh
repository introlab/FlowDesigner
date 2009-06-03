#!/bin/sh

FLOWDESIGNER_BUNDLE="`echo "$0" | sed -e 's/\/Contents\/MacOS\/FlowDesigner//'`"

echo "FlowDesigner running from  : $PWD   Bundle : $FLOWDESIGNER_BUNDLE"

export DYLD_LIBRARY_PATH=$FLOWDESIGNER_BUNDLE/Contents/Resources/lib
export FLOWDESIGNER_PATH=$FLOWDESIGNER_BUNDLE/Contents/Resources/lib/flowdesigner
export PATH=$FLOWDESIGNER_BUNDLE/Contents/Resources/bin:$PATH

#run
$FLOWDESIGNER_BUNDLE/Contents/Resources/bin/flowdesigner
