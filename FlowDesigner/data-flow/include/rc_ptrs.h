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

template <class X>
class counted_pod_ptr
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
   explicit counted_pod_ptr(X* p=0) : ptr(p)
   {
      count=new size_type(1);
   }
   

   //template <class Y>
   typedef X Y;
   counted_pod_ptr(const counted_pod_ptr<Y> &r)
   {
      ptr=r.ptr;
      count=r.count;
      acquire();
   }
   
   ~counted_pod_ptr() { release(); }
   

   //template <class Y>

   counted_pod_ptr& operator= (const counted_pod_ptr<Y> &r)
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

   //template <class Y>

   void acquire()
   {
      ++(*count);
   }
};




#define counted_ptr counted_pod_ptr
/*
template <class X>
class counted_ptr : public counted_pod_ptr<X>
{
public:
   explicit counted_ptr(X* p=0) : counted_pod_ptr<X>(p){}


   template <class Y>

   counted_ptr(const counted_ptr<Y> &r)
      : counted_pod_ptr<X>(r) {}

   ~counted_ptr()	{ }


   template <class Y>

   counted_ptr& operator= (const counted_ptr<Y> &r)
   {
      counted_pod_ptr<Y>::operator= (r);
      return *this;
   }

   X* operator-> () const { return ptr; }
};

*/






//
// counted_pod_array_ptr
//

template <class X>
class counted_pod_array_ptr
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
   explicit counted_pod_array_ptr(X* p=0) : ptr(p)
   {
      count=new size_type; *count=1;
   }

#if defined(__XC_SUPPORTS_MEMBER_TEMPLATES)
   template <class Y>
#else
   typedef X Y;
#endif
   counted_pod_array_ptr(const counted_pod_array_ptr<Y> &r)
   {
      ptr=r.ptr;
      count=r.count;
      (*count)++;
   }

   ~counted_pod_array_ptr() {	release(); }

#if defined(__XC_SUPPORTS_MEMBER_TEMPLATES)
   template <class Y>
#endif
   counted_pod_array_ptr& operator= (const counted_pod_array_ptr<Y> &r)
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
   X* get () const {	return ptr;	}
   bool unique () const	{ return *count==1; }

protected:

   void release()
   {
      if (count && --(*count)==0)
      {
         delete [] ptr;
         delete count;
      }
      ptr = 0;
      count = 0;
   }
#if defined(__XC_SUPPORTS_MEMBER_TEMPLATES)
   template <class Y>
#endif
   void acquire()
   {
      ++(*count);
   }
};


/*
template <class X>
class counted_array_ptr : public counted_pod_array_ptr<X>
{
public:
   explicit counted_array_ptr(X* p=0) : counted_pod_array_ptr<X>(p){}

#if defined(__XC_SUPPORTS_MEMBER_TEMPLATES)
   template <class Y>
#else
   typedef X Y;
#endif
   counted_array_ptr(const counted_array_ptr<Y> &r)
      : counted_pod_array_ptr<X>(r) {}

   ~counted_array_ptr()	{ }

#if defined(__XC_SUPPORTS_MEMBER_TEMPLATES)
   template <class Y>
#endif
   counted_array_ptr& operator= (const counted_array_ptr<Y> &r)
   {
      counted_pod_array_ptr<Y>::operator= (r);
      return *this;
   }

   X& operator* () const {	return *ptr; }

   X* operator-> () const { return ptr; }

};
*/

#endif

