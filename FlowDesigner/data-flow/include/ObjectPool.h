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

#ifndef OBJECT_POOL_H
#define OBJECT_POOL_H

#include <vector>

using namespace std;

#define MAX_STORE 100

template <class T>
class ObjectPool {
  protected:
   static vector <T *> stack;

  public:
   ObjectPool() 
   {
      //Do not put anything there... everything is static!
   }

   ~ObjectPool() 
   {
      //Do not put anything there... everything is static!
   }

   static T *alloc()
   {
      if (stack.size())
      {
	 T *ret = stack.back();
	 stack.pop_back();
	 ret->ref();
	 return ret;
      } else {
	 return new T;
      }
   }

   static void release(T *obj)
   {
      if (stack.size() > MAX_STORE)
      {
	 delete obj;
      } else {
	 obj->status = Object::valid;
	 stack.push_back(obj);
      }
   }

};

#endif
