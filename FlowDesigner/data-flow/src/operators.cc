// Copyright (C) 1999 Jean-Marc Valin
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include "operators.h"
#include "net_types.h"


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



/**
New type addition
Dominic Letourneau
05/03/2001
**/

/*

//Operators for Short and Short
ObjectRef addShortShort(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Short(dereference_cast<short> (x) + (dereference_cast<short> (y))));
}
REGISTER_DOUBLE_VTABLE(addVtable, addShortShort, Short, Short);

ObjectRef mulShortShort(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Short(dereference_cast<short> (x) * (dereference_cast<short> (y))));
}
REGISTER_DOUBLE_VTABLE(mulVtable, mulShortShort, Short, Short);

ObjectRef subShortShort(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Short(dereference_cast<short> (x) - (dereference_cast<short> (y))));
}
REGISTER_DOUBLE_VTABLE(subVtable, subShortShort, Short, Short);

ObjectRef divShortShort(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Short(dereference_cast<short> (x) / (dereference_cast<short> (y))));
}
REGISTER_DOUBLE_VTABLE(divVtable, divShortShort, Short, Short);



//Operators for Short and Int
ObjectRef addShortInt(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Int(dereference_cast<short> (x) + (dereference_cast<int> (y))));
}
REGISTER_DOUBLE_VTABLE(addVtable, addShortInt, Short, Int);

ObjectRef mulShortInt(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Int(dereference_cast<short> (x) * (dereference_cast<int> (y))));
}
REGISTER_DOUBLE_VTABLE(mulVtable, mulShortInt, Short, Int);

ObjectRef subShortInt(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Int(dereference_cast<short> (x) - (dereference_cast<int> (y))));
}
REGISTER_DOUBLE_VTABLE(subVtable, subShortInt, Short, Int);

ObjectRef divShortInt(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Int(dereference_cast<short> (x) / (dereference_cast<int> (y))));
}
REGISTER_DOUBLE_VTABLE(divVtable, divShortInt, Short, Int);


//Operators for Int and Short
ObjectRef addIntShort(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Int(dereference_cast<int> (x) + (dereference_cast<short> (y))));
}
REGISTER_DOUBLE_VTABLE(addVtable, addIntShort, Int, Short);

ObjectRef mulIntShort(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Int(dereference_cast<int> (x) * (dereference_cast<short> (y))));
}
REGISTER_DOUBLE_VTABLE(mulVtable, mulIntShort, Int, Short);

ObjectRef subIntShort(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Int(dereference_cast<int> (x) - (dereference_cast<short> (y))));
}
REGISTER_DOUBLE_VTABLE(subVtable, subIntShort, Int, Short);

ObjectRef divIntShort(ObjectRef x, ObjectRef y)
{
   return ObjectRef(new Int(dereference_cast<int> (x) / (dereference_cast<short> (y))));
}
REGISTER_DOUBLE_VTABLE(divVtable, divIntShort, Int, Short);

*/
