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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
//#include "Vector.h"
//#define NO_HASH_MAP

#ifdef NO_HASH_MAP
#include <map>
#else
#ifdef HAVE_HASH_MAP
#include <hash_map>
#elif defined (HAVE_EXT_HASH_MAP)
#include <ext/hash_map>
#endif
#endif

using namespace std;

#define MAX_SMALL 512


template <class T>
class VectorPool {
  protected:
   int max_stored;

   vector<vector <Vector<T> *> > smallList;
#ifdef NO_HASH_MAP
   map<int, vector <Vector<T> *> > stackList;
#else
   //hash_map<int, vector <Vector<T> *>, hash<int> > stackList;
   hash_map<int, vector <Vector<T> *>, hash<int> > stackList;
#endif

  public:
   VectorPool(int _max_stored=50) 
      : max_stored(_max_stored)
      , smallList (MAX_SMALL+1)
   {}

   //vector <Vector<T> *> &operator [] 
   Vector<T> *newVector (int size)
   {
      if (size <= MAX_SMALL)
      {
	 vector <Vector<T> *> &stack = smallList[size];
	 if (stack.empty())
	 {
	    return new Vector<T> (size);
	 } else {
	    Vector<T> *ret = stack.back();
	    stack.pop_back();
	    ret->ref();
	    
	    return ret;
	 }
      } else {
	 vector <Vector<T> *> &stack = stackList[size];
	 if (stack.empty())
	 {
	    return new Vector<T> (size);
	 } else {
	    Vector<T> *ret = stack.back();
	    stack.pop_back();
	    ret->ref();
	    
	    return ret;
	    
	 }
      }
   }
   void release(Vector<T> *vec)
   {
      int sz = vec->size();
      if (sz <= MAX_SMALL)
      {
	 vector <Vector<T> *> &stack = smallList[sz];
	 if (stack.size() > max_stored)
	 {
	    delete vec;
	 } else {
	    vec->status = Object::valid;
	    stack.push_back(vec);
	 }
	 
      } else {
	 //cerr << "send on stack\n";
	 vector <Vector<T> *> &stack = stackList[sz];
	 //deque <Vector<T> *> &stack = stackList[vec->size()];
	 if (stack.size() > max_stored)
	 {
	    delete vec;
	 } else {
	    vec->status = Object::valid;
	    stack.push_back(vec);
	    //stack.insert(stack.end(), vec);
	 }
      }
   }
};

#endif
