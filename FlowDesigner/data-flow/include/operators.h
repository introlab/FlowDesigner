// Copyright (C) 1999 Jean-Marc Valin

#include "DoubleDispatch.h"
#include "misc.h"

DEFINE_DOUBLE_VTABLE(addVtable);

inline ObjectRef operator+(ObjectRef x, ObjectRef y)
{
   return addVtable::perform(x,y);
}

DEFINE_DOUBLE_VTABLE(subVtable);

inline ObjectRef operator-(ObjectRef x, ObjectRef y)
{
   return subVtable::perform(x,y);
}

DEFINE_DOUBLE_VTABLE(mulVtable);

inline ObjectRef operator*(ObjectRef x, ObjectRef y)
{
   return mulVtable::perform(x,y);
}

DEFINE_DOUBLE_VTABLE(divVtable);

inline ObjectRef operator/(ObjectRef x, ObjectRef y)
{
   return divVtable::perform(x,y);
}

DEFINE_DOUBLE_VTABLE(smallerVtable);

inline ObjectRef operator<(ObjectRef x, ObjectRef y)
{
  return smallerVtable::perform(x,y);
}

inline ObjectRef operator>(ObjectRef x, ObjectRef y)
{
  return smallerVtable::perform(y,x);
}


DEFINE_DOUBLE_VTABLE(equalVtable);

inline ObjectRef operator==(ObjectRef x, ObjectRef y)
{
   return equalVtable::perform(x,y);
}


DEFINE_DOUBLE_VTABLE(maxVtable);
inline ObjectRef max(ObjectRef x, ObjectRef y)
{
   return maxVtable::perform(x,y);
}


DEFINE_DOUBLE_VTABLE(minVtable);
inline ObjectRef min(ObjectRef x, ObjectRef y)
{
   return minVtable::perform(x,y);
}


DEFINE_DOUBLE_VTABLE(concatVtable);
inline ObjectRef concat(ObjectRef x, ObjectRef y)
{
   return concatVtable::perform(x,y);
}

//let's try that (DL 22/07/2003)
DEFINE_SINGLE_VTABLE(convertToIntVtable);
inline ObjectRef convertToInt(ObjectRef x)
{
   return convertToIntVtable::perform(x);
}

DEFINE_SINGLE_VTABLE(convertToFloatVtable);
inline ObjectRef convertToFloat(ObjectRef x)
{
   return convertToFloatVtable::perform(x);
}

DEFINE_SINGLE_VTABLE(convertToVectorIntVtable);
inline ObjectRef convertToVectorInt(ObjectRef x)
{
   return convertToVectorIntVtable::perform(x);
}

DEFINE_SINGLE_VTABLE(convertToVectorFloatVtable);
inline ObjectRef convertToVectorFloat(ObjectRef x)
{
   return convertToVectorFloatVtable::perform(x);
}



