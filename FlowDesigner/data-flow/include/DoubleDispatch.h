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

#ifndef DOUBLE_DISPATCH_H
#define DOUBLE_DISPATCH_H

#include <map>

//template<class T, class U>
class DoubleDispatch {
  public:
   typedef ObjectRef (*funct_ptr) (ObjectRef x, ObjectRef y);

  protected:
   typedef map<type_info *, funct_ptr> vtable1Type;
   typedef map<type_info *, vtable1Type > vtable2Type;
   vtable2Type vtable;

  public:
   void registerFunct(funct_ptr ptr, type_info *x, type_info *y)
   {
      map[x][y] = ptr;
   }

   ObjectRef call(ObjectRef x, ObjectRef y)
   {
      type_info *t1 = &typeid(x);
      type_info *t2 = &typeid(y);
      vtable2Type::iterator v1 = vtable.find(t1);
      if (v1)
      {
	 vtable1Type::iterator v2 = v1->second.find(t2);
	 if (v2)
	 {
	    return v2->second(x,y);
	 } else {
	    cerr << "vtable error ptr2 is null\n";
	 }
      } else {
	 cerr << "vtable error ptr1 is null\n";
      }
   }
};

#endif
