// Copyright (C) 1999 Jean-Marc Valin

#include "operators.h"
#include "net_types.h"

//@implements core

//Operators for Float and Float
ObjectRef addFloatFloat(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Float(dereference_cast<float> (x) + (dereference_cast<float> (y))));
}
REGISTER_DOUBLE_VTABLE(addVtable, addFloatFloat, Float, Float);

ObjectRef mulFloatFloat(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Float(dereference_cast<float> (x) * (dereference_cast<float> (y))));
}
REGISTER_DOUBLE_VTABLE(mulVtable, mulFloatFloat, Float, Float);

ObjectRef subFloatFloat(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Float(dereference_cast<float> (x) - (dereference_cast<float> (y))));
}
REGISTER_DOUBLE_VTABLE(subVtable, subFloatFloat, Float, Float);

ObjectRef divFloatFloat(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Float(dereference_cast<float> (x) / (dereference_cast<float> (y))));
}
REGISTER_DOUBLE_VTABLE(divVtable, divFloatFloat, Float, Float);

ObjectRef smallerFloatFloat(ObjectRef x, ObjectRef y)
{

  if (dereference_cast<float> (x) < (dereference_cast<float> (y))) {
    return TrueObject;
  }
  else {
    return FalseObject;
  }
}
REGISTER_DOUBLE_VTABLE(smallerVtable, smallerFloatFloat, Float, Float);

//FIXME: Should we really return the object of should we copy it?
ObjectRef maxFloatFloat(ObjectRef x, ObjectRef y)
{

  if (dereference_cast<float> (x) < (dereference_cast<float> (y))) {
    return y;
  }
  else {
    return x;
  }
}
REGISTER_DOUBLE_VTABLE(maxVtable, maxFloatFloat, Float, Float);

ObjectRef minFloatFloat(ObjectRef x, ObjectRef y)
{

  if (dereference_cast<float> (x) > (dereference_cast<float> (y))) {
    return y;
  }
  else {
    return x;
  }
}
REGISTER_DOUBLE_VTABLE(minVtable, minFloatFloat, Float, Float);


//Operators for Int and Int
ObjectRef addIntInt(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Int(dereference_cast<int> (x) + (dereference_cast<int> (y))));
}
REGISTER_DOUBLE_VTABLE(addVtable, addIntInt, Int, Int);

ObjectRef mulIntInt(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Int(dereference_cast<int> (x) * (dereference_cast<int> (y))));
}
REGISTER_DOUBLE_VTABLE(mulVtable, mulIntInt, Int, Int);

ObjectRef subIntInt(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Int(dereference_cast<int> (x) - (dereference_cast<int> (y))));
}
REGISTER_DOUBLE_VTABLE(subVtable, subIntInt, Int, Int);

ObjectRef divIntInt(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Int(dereference_cast<int> (x) / (dereference_cast<int> (y))));
}
REGISTER_DOUBLE_VTABLE(divVtable, divIntInt, Int, Int);

ObjectRef smallerIntInt(ObjectRef x, ObjectRef y)
{
  if (dereference_cast<int> (x) < (dereference_cast<int> (y))) {
    return TrueObject;
  }
  else {
    return FalseObject;
  }
}
REGISTER_DOUBLE_VTABLE(smallerVtable, smallerIntInt, Int, Int);

ObjectRef maxIntInt(ObjectRef x, ObjectRef y)
{
  if (dereference_cast<int> (x) < (dereference_cast<int> (y))) {
    return y;
  }
  else {
    return x;
  }
}
REGISTER_DOUBLE_VTABLE(maxVtable, maxIntInt, Int, Int);

ObjectRef minIntInt(ObjectRef x, ObjectRef y)
{
  if (dereference_cast<int> (x) > (dereference_cast<int> (y))) {
    return y;
  }
  else {
    return x;
  }
}
REGISTER_DOUBLE_VTABLE(minVtable, minIntInt, Int, Int);




//Operators for Int and Float
ObjectRef addIntFloat(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Float(dereference_cast<int> (x) + (dereference_cast<float> (y))));
}
REGISTER_DOUBLE_VTABLE(addVtable, addIntFloat, Int, Float);

ObjectRef mulIntFloat(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Float(dereference_cast<int> (x) * (dereference_cast<float> (y))));
}
REGISTER_DOUBLE_VTABLE(mulVtable, mulIntFloat, Int, Float);

ObjectRef subIntFloat(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Float(dereference_cast<int> (x) - (dereference_cast<float> (y))));
}
REGISTER_DOUBLE_VTABLE(subVtable, subIntFloat, Int, Float);

ObjectRef divIntFloat(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Float(dereference_cast<int> (x) / (dereference_cast<float> (y))));
}
REGISTER_DOUBLE_VTABLE(divVtable, divIntFloat, Int, Float);

ObjectRef smallerIntFloat(ObjectRef x, ObjectRef y)
{
  if (dereference_cast<int> (x) < (dereference_cast<float> (y))) {
    return TrueObject;
  }
  else {
    return FalseObject;
  }
}
REGISTER_DOUBLE_VTABLE(smallerVtable, smallerIntFloat, Int, Float);



//Operators for Float and Int
ObjectRef addFloatInt(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Float(dereference_cast<float> (x) + (dereference_cast<int> (y))));
}
REGISTER_DOUBLE_VTABLE(addVtable, addFloatInt, Float, Int);

ObjectRef mulFloatInt(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Float(dereference_cast<float> (x) * (dereference_cast<int> (y))));
}
REGISTER_DOUBLE_VTABLE(mulVtable, mulFloatInt, Float, Int);

ObjectRef subFloatInt(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Float(dereference_cast<float> (x) - (dereference_cast<int> (y))));
}
REGISTER_DOUBLE_VTABLE(subVtable, subFloatInt, Float, Int);

ObjectRef divFloatInt(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Float(dereference_cast<float> (x) / (dereference_cast<int> (y))));
}
REGISTER_DOUBLE_VTABLE(divVtable, divFloatInt, Float, Int);

ObjectRef smallerFloatInt(ObjectRef x, ObjectRef y)
{
  if (dereference_cast<float> (x) < (dereference_cast<int> (y))) {
    return TrueObject;
  }
  else {
    return FalseObject;
  }
}
REGISTER_DOUBLE_VTABLE(smallerVtable, smallerFloatInt, Float, Int);



ObjectRef equalInt(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Bool(dereference_cast<int> (x) == (dereference_cast<int> (y))));
}
REGISTER_DOUBLE_VTABLE(equalVtable, equalInt, Int, Int);

ObjectRef equalString(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Bool(object_cast<String> (x) == (object_cast<String> (y))));
}
REGISTER_DOUBLE_VTABLE(equalVtable, equalString, String, String);



ObjectRef concatString(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new String(object_cast<String> (x) + (object_cast<String> (y))));
}
REGISTER_DOUBLE_VTABLE(concatVtable, concatString, String, String);
