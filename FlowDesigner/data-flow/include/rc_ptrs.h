#ifndef __RC_PTRS_H
#define __RC_PTRS_H

/*
 * Copyright 1998
 * Mark E. (snowball3@usa.net)
 * http://members.xoom.com/snowball3/cpp/
 *
 * Permission to use, copy, modify, and distribute this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.
 *
 * Mark E. makes no representations about the suitability of
 * this software for any purpose. It is provided "as is" without
 * express or implied warranty.
 *
 */

// Implemented are four classes:
// counted_ptr, counted_pod_ptr, counted_array_ptr, and counted_pod_array_ptr
// These classes implement reference-counted smart pointers that automatically
// deletes the pointer it contains when no longer needed
// i.e. (reference count drops to zero)

#include <stddef.h>
#include <stream.h>
#include <typeinfo>

template <class X>
class Ptr
{
//
// Public typedefs
//
public:
   typedef X element_type;
   typedef X* pointer_type;
   typedef size_t size_type;

protected:
   X* ptr;
   size_type *count;

   
public:
   explicit Ptr(X* p=0) : ptr(p)
   {
      //cerr << "alloc " << typeid(X).name() << endl;
      count=new size_type(1);
   }
   
   template <class Z>
   Ptr(const Ptr<Z> &r)
   {
      cerr << "type-safe copy constructor" << endl;
      ptr=dynamic_cast<X> (r.ptr);
      if (!ptr) throw "Ptr<X>: Illegal pointer conversion in copy constructor";
      count=r.count;
      acquire();
   }
   
   //template <class Y>
   typedef X Y;
   Ptr(const Ptr<Y> &r)
   {
      ptr=r.ptr;
      count=r.count;
      acquire();
   }
   
   ~Ptr() { release(); }
   

   //template <class Y>

   Ptr& operator= (const Ptr<Y> &r)
   {
      if (this != &r)
      {
         release();
         ptr = r.ptr;
         count = r.count;
         acquire();
      }
      return *this;
   }

   X& operator* () const {	return *ptr; }
   X* operator->() const {	return  ptr; }
   X* get () const { return ptr;	}

   bool unique () const
   {
      return *count==1;
   }

   X *detach () 
   {
      if (count)
      {
         if (*count==1) 
         {
            X *tmp = ptr;
            delete count;
            count = 0;
            ptr = 0;
            return tmp;
         } else {
            throw "Error: trying to detach a non-unique pointer in rc_ptrs.h";
         }
      } else {
         throw "Error: trying to detach a NULL pointer in rc_ptrs.h";
      }
   }

protected:
   void release()
   {
      if (count && --(*count)==0)
      {
         //cerr << "delete " << typeid(X).name() << endl;
         delete ptr;
         delete count;
      }
      ptr = 0;
      count = 0;
   }

   //template <class Y>

   void acquire()
   {
      ++(*count);
   }
};


#endif

