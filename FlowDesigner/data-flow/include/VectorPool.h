// Copyright (C) 1999-2001 Jean-Marc Valin

#ifndef VECTOR_POOL_H
#define VECTOR_POOL_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "misc.h"
#include "multithread.h"

using namespace std;

#define MAX_SMALL 512
#define MAX_BITS 32


template <class T>
class VectorPool {
  protected:
   size_t max_stored;

   vector<vector <Vector<T> *> > smallList;
   vector<vector <Vector<T> *> > largeList;
   FastMutex mutex;

  public:
   VectorPool(int _max_stored=50) 
      : max_stored(_max_stored)
      , smallList (MAX_SMALL+1)
      , largeList (MAX_BITS+1)
   {}

   Vector<T> *newVector (int size)
   {
      mutex.lock();
      if (size <= MAX_SMALL)
      {
	 vector <Vector<T> *> &stack = smallList[size];
	 if (stack.empty())
	 {
            mutex.unlock();
	    return new Vector<T> (size);
	 } else {
	    Vector<T> *ret = stack.back();
	    stack.pop_back();
	    ret->ref();
            mutex.unlock();	    
	    return ret;
	 }
      } else {
	 vector <Vector<T> *> &stack = largeList[log2(size)];
	 if (stack.empty())
	 {
            mutex.unlock();
	    return new Vector<T> (size);
	 } else {
	    Vector<T> *ret = stack.back();
	    stack.pop_back();
	    ret->ref();
	    ret->resize(size);
            mutex.unlock();
	    return ret;
	    
	 }
      }
   }
   void release(Vector<T> *vec)
   {
      mutex.lock();
      int sz = vec->size();
      if (sz <= MAX_SMALL)
      {
	 vector <Vector<T> *> &stack = smallList[sz];
	 if (stack.size() > max_stored)
	 {
	    delete vec;
	 } else {
	    stack.push_back(vec);
	 }
	 
      } else {
	 vector <Vector<T> *> &stack = largeList[log2(sz)];
	 if (stack.size() > max_stored)
	 {
	    delete vec;
	 } else {
	    stack.push_back(vec);
	 }
      }
      mutex.unlock();
   }
};

#endif
