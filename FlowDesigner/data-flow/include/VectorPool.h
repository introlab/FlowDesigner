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

#include "Vector.h"

template <class T>
class VectorPool {
  protected:
   map<int, vector <Vector<T> *> > stackList;
  public:
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
	 return ret;
	 
      }
   }
   void release(Vector<T> *vec)
   {
      //cerr << "send on stack\n";
      vector <Vector<T> *> &stack = stackList[vec->size()];
      stack.insert(stack.end(), vec);
   }
};
