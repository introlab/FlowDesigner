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
      count=new size_type(1);
   }
   
   template <class Z>
   Ptr(const Ptr<Z> &r)
   {
      ptr=dynamic_cast<X*> (r.ptr);
      if (!ptr) throw "Ptr<X>: Illegal pointer conversion in copy constructor";
      count=r.count;
      acquire();
   }
   
   Ptr(const Ptr<X> &r)
   {
      ptr=r.ptr;
      count=r.count;
      acquire();
   }
   
   ~Ptr() { release(); }
   

   template <class Z>
   Ptr& operator= (const Ptr<Z> &r)
   {
      if ((int) this != (int) (&r))
      {
         release();
         ptr=dynamic_cast<X*> (r.ptr);
         if (!ptr) throw "Ptr<X>: Illegal pointer conversion in operator =";
         count = r.count;
         acquire();
      }
      return *this;
   }

   Ptr& operator= (const Ptr<X> &r)
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

#ifdef RT_DEBUG
   X& operator* () const {if (ptr) return *ptr; else throw "dereferencing NULL pointer in *";}
   X* operator->() const {if (ptr) return  ptr; else throw "dereferencing NULL pointer in ->";}
#else
   X& operator* () const {	return *ptr; }
   X* operator->() const {	return  ptr; }
#endif
   X* get () const { return ptr;}

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
         delete ptr;
         delete count;
      }
      ptr = 0;
      count = 0;
   }

   void acquire()
   {
      ++(*count);
   }

   template <class Z>
   friend class Ptr;
};


#endif

