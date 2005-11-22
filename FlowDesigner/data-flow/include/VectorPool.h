// Copyright (C) 1999-2001 Jean-Marc Valin

#ifndef VECTOR_POOL_H
#define VECTOR_POOL_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "misc.h"
#include "multithread.h"

namespace FD {

  //forward declaration of class Vector
template <class T>
class Vector;

#define MAX_SMALL 512
#define MAX_BITS 32

template <class T>
class VectorPool {
  protected:
   size_t max_stored;

   std::vector<std::vector <Vector<T> *> > smallList;
   std::vector<std::vector <Vector<T> *> > largeList;
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
	 std::vector <Vector<T> *> &stack = smallList[size];
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
#ifndef __CYGWIN__
	 std::vector <Vector<T> *> &stack = largeList[FD::log2(size)];
#else
	 std::vector <Vector<T> *> &stack = largeList[(int) log2(size)];
#endif
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
	 std::vector <Vector<T> *> &stack = smallList[sz];
	 if (stack.size() > max_stored)
	 {
	    delete vec;
	 } else {
	    stack.push_back(vec);
	 }
	 
      } else {
#ifndef __CYGWIN__
	std::vector <Vector<T> *> &stack = largeList[FD::log2(sz)];
#else
	std::vector <Vector<T> *> &stack = largeList[(int) log2(sz)];
#endif
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

}//namespace FD
#endif
