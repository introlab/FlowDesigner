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

#include <ObjectRef.h>
#include "BaseException.h"

//#ifdef NO_HASH_MAP
#include <map>
//#else
//#include <hash_map>
//#endif

class DoubleDispatch;

class DoubleDispatchException : public BaseException {
  protected:
   DoubleDispatch *table;
   string type1, type2;
  public:
   DoubleDispatchException(DoubleDispatch *_table, string _type1, string _type2);
   virtual void print(ostream &out = cerr);
   
};

//template<class T, class U>
class DoubleDispatch {
  public:
   typedef ObjectRef (*funct_ptr) (ObjectRef x, ObjectRef y);

  protected:
   string name;
//#ifdef NO_HASH_MAP
   typedef map<const type_info *, funct_ptr> vtable1Type;
   typedef map<const type_info *, vtable1Type > vtable2Type;
//#else
//   typedef hash_map<const type_info *, funct_ptr> vtable1Type;
//   typedef hash_map<const type_info *, vtable1Type > vtable2Type;
//#endif
   vtable2Type vtable;

  public:
   DoubleDispatch(string _name) : name(_name) {}

   const string &getName() {return name;}

   void registerFunct(funct_ptr ptr, const type_info *x, const type_info *y)
   {
      vtable[x][y] = ptr;
   }

   ObjectRef call(ObjectRef x, ObjectRef y)
   {
      const type_info *t1 = &typeid(*x);
      const type_info *t2 = &typeid(*y);
      vtable2Type::iterator v1 = vtable.find(t1);
      if (v1!=vtable.end())
      {
	 vtable1Type::iterator v2 = v1->second.find(t2);
	 if (v2!=v1->second.end())
	 {
	    return v2->second(x,y);
	 } else {
	    throw new DoubleDispatchException(this, t1->name(), t2->name());
	 }
      } else {
	 throw new DoubleDispatchException(this, t1->name(), t2->name());
      }
   }
};


#define DEFINE_DOUBLE_VTABLE(klass) class klass {                                   \
  public:                                                                           \
   static DoubleDispatch &vtable() {static DoubleDispatch table(# klass); return table;}     \
   static ObjectRef perform(ObjectRef x, ObjectRef y)                               \
   {                                                                                \
      return vtable().call(x,y);                                                    \
   }                                                                                \
   static int reg(DoubleDispatch::funct_ptr ptr, const type_info *x, const type_info *y) \
   {                                                                                \
      vtable().registerFunct(ptr,x,y);                                              \
      return 0;                                                                     \
   }                                                                                \
};

#define REGISTER_DOUBLE_VTABLE(klass, func, type1, type2) \
        int dummy_vtable_init_for ## klass ## _ ## func =\
        klass::reg(func, &typeid(type1), &typeid(type2));

#endif