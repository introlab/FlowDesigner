// Copyright (C) 2003 Dominic Letourneau (dominic.letourneau@usherbrooke.ca)

#include "operators.h"
#include "net_types.h"

//@implements core

/*
 *Type conversion from one type to another. Should be implemented otherwise?
 * (DL) July 22 2003
*/

/*
//Conversion from Int to Int (useless?)
ObjectRef convertIntToInt(ObjectRef x)
{
   return ObjectRef(Int::alloc(dereference_cast<int> (x)));
}
REGISTER_SINGLE_VTABLE(convertToIntVtable, convertIntToInt, Int);


//Conversion from Float to Int
ObjectRef convertFloatToInt(ObjectRef x)
{
   return ObjectRef(Int::alloc((int)dereference_cast<float> (x)));
}
REGISTER_SINGLE_VTABLE(convertToIntVtable, convertFloatToInt, Float);

//Conversion from Int to Float
ObjectRef convertIntToFloat(ObjectRef x)
{
   return ObjectRef(Float::alloc((float)dereference_cast<int> (x)));
}
REGISTER_SINGLE_VTABLE(convertToFloatVtable, convertIntToFloat, Int);


//Conversion from Float to Float (useless?)
ObjectRef convertFloatToFloat(ObjectRef x)
{
   return ObjectRef(Float::alloc(dereference_cast<float> (x)));
}
REGISTER_SINGLE_VTABLE(convertToFloatVtable, convertFloatToFloat, Float);
*/
