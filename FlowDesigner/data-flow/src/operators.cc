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
