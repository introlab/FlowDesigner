// Copyright (C) 1999 Jean-Marc Valin

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
	 stack.push_back(obj);
      }
   }

};

#endif
