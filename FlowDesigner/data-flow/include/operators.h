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

#include "DoubleDispatch.h"

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

