#!/bin/sh

if batchflow factorial.n | grep -q '^<Int 120 >$';
then
echo factorial.n passed
else
echo factorial.n failed
fi

if batchflow exception.n | grep -q '^<Int 9 >$';
then
echo exception.n passed
else
echo exception.n failed
fi

if batchflow vect.n | grep -q '<Vector<ObjectRef> <String Hello World! > <Vector<float> 5 7 9 >  <Vector<float> 3 9 5 >  >';
then
echo vect.n passed
else
echo vect.n failed
fi

if batchflow saveload.n | grep -q '<Vector<ObjectRef> <String HelloWorld! > <Vector<float> 5 7 9 >  <Vector<float> 3 9 5 >  >';
then
echo saveload.n passed
else
echo saveload.n failed
fi

if batchflow serial.n | grep -q '<Vector<ObjectRef> <String HelloWorld! > <Vector<float> 5 7 9 >  <Vector<float> 3 9 5 >  >';
then
echo serial.n passed
else
echo serial.n failed
fi
