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

#ifndef VECTOR_POOL_H
#define VECTOR_POOL_H

//#include "Vector.h"

#ifdef NO_HASH_MAP
#include <map>
#else
#include <hash_map>
#endif

template <class T>
class VectorPool {
  protected:
   int max_stored;

#ifdef NO_HASH_MAP
   map<int, vector <Vector<T> *> > stackList;
#else
   hash_map<int, vector <Vector<T> *>, hash<int> > stackList;
#endif

  public:
   VectorPool(int _max_stored=50) 
      : max_stored(_max_stored)
   {}

   //vector <Vector<T> *> &operator [] 
   Vector<T> *newVector (int size)
   {
      vector <Vector<T> *> &stack = stackList[size];
      if (stack.size() == 0)
      {
	 //cerr << "alloc new\n";
	 return new Vector<T> (size);
      } else {
	 //cerr << "return old\n";
	 int sz = stack.size();
	 Vector<T> *ret = stack[sz-1];
	 stack.resize(sz-1);
	 ret->ref();
	 return ret;
	 
      }
   }
   void release(Vector<T> *vec)
   {
      //cerr << "send on stack\n";
      vector <Vector<T> *> &stack = stackList[vec->size()];
      if (stack.size() > max_stored)
      {
	 delete vec;
      } else {
	 vec->status = Object::valid;
	 stack.insert(stack.end(), vec);
      }
   }
};

#endif
