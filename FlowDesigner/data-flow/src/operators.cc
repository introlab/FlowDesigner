// Copyright (C) 1999 Jean-Marc Valin

#include "operators.h"
#include "net_types.h"

//@implements core

using namespace std;

namespace FD {



  //DL TODO :
  //This should be automated in some way
  //December 7, 2005

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
   if (dereference_cast<int> (x) == (dereference_cast<int> (y)))
      return TrueObject;
   else
      return FalseObject;
}
REGISTER_DOUBLE_VTABLE(equalVtable, equalInt, Int, Int);

ObjectRef equalString(ObjectRef x, ObjectRef y)
{
   if (object_cast<String> (x) == (object_cast<String> (y)))
      return TrueObject;
   else
      return FalseObject;
}
REGISTER_DOUBLE_VTABLE(equalVtable, equalString, String, String);



ObjectRef concatString(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new String(object_cast<String> (x) + (object_cast<String> (y))));
}
REGISTER_DOUBLE_VTABLE(concatVtable, concatString, String, String);

}//namespace FD
