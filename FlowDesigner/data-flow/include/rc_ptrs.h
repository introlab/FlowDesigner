// Copyright (C) 2001 Jean-Marc Valin

#ifndef __RC_PTRS_H
#define __RC_PTRS_H

#ifdef _MSC_VER
#pragma warning (disable: 4786)
#endif

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

/*
   Since then, heavily modified by Jean-Marc Valin (1999)
   valj01@gel.usherb.ca
*/



#include <stddef.h>
#include <iostream>
#include <typeinfo>
#include "BaseException.h"
#include <string>

#ifdef WIN32 /*Work around bug in MSVC++ (for) variable scope*/
#define for if(0);else for
#endif

namespace FD {

/** 
    The PtrCastException occurs when we are unable to cast a RCPtr<T>
    to a RCPtr<U>
    @author Jean-Marc Valin.
    @version 1.0
 */
template <class T, class U>
class PtrCastException : public GenericCastException {
protected:
	std::string type;
public:
   /**The constructur that takes an error message*/
   PtrCastException(const T *obj)
      : type(typeid(*obj).name())
   {}
  
   /**The print method*/
   virtual void print(std::ostream &out = std::cerr) 
   {
      out << "Cast error: Trying to cast RCPtr <" << typeid(T).name() << "> (" 
          << type << ") into RCPtr<" << typeid(U).name() << ">" << std::endl;
   }
};

class PtrException : public BaseException {
protected:
	std::string message;
public:
   /**The constructur that takes an error message*/
   PtrException(const std::string &_message)
      : message(_message)
   {}
  
   /**The print method*/
   virtual void print(std::ostream &out = std::cerr) 
   {
      out << message << std::endl;
   }
};


/** 
    The counted pointer class
    @author Jean-Marc Valin.
    @version 1.0
 */
template <class X>
class RCPtr
{
public:
   typedef X element_type;
   typedef X* pointer_type;
   typedef size_t size_type;

#ifndef BROKEN_TEMPLATES
protected:
#endif
   X* ptr;
   //size_type *count;
   

public:
   explicit RCPtr(X* p=0) : ptr(p)
   {
      //count=new size_type(1);
   }

   bool isNil() {return ptr == 0;}

   template <class Z>
     RCPtr(const RCPtr<Z> &r);

   /*{
     cerr << "RCPtr(const RCPtr<Z> &r)" << std::endl;
      ptr=dynamic_cast<X*> (r.ptr);
      if (!ptr) 
         {
            //throw "RCPtr<X>: Illegal pointer conversion in copy constructor";
            if (!ptr) throw new PtrCastException<Z,X>(r.ptr);
         }
      //count=r.count;
      acquire();
      }*/
   
   RCPtr(const RCPtr<X> &r)
   {
      ptr=r.ptr;
      //count=r.count;
      acquire();
   }
   
   ~RCPtr() { release(); }
   

   template <class Z>
   RCPtr& operator= (const RCPtr<Z> &r)
      ;/*{
      if ((void*) this != (void*) (&r))
      {
         X *tmp=dynamic_cast<X*> (r.ptr);
         //if (!tmp) throw "RCPtr<X>: Illegal pointer conversion in operator =";
         if (!tmp) throw new PtrCastException<Z,X>(r.ptr);
         release();
         ptr=tmp;
         //count = r.count;
         acquire();
      }
      return *this;
      }*/

   RCPtr& operator= (const RCPtr<X> &r)
   {
      if (this != &r)
      {
         release();
         ptr = r.ptr;
         //count = r.count;
         acquire();
      }
      return *this;
   }

   template <class Z>
   RCPtr& operator= (Z *r)
   {
      if ((void*) ptr != (void*) (r))
      {
         X *tmp=dynamic_cast<X*> (r);
         //if (!tmp) throw "RCPtr<X>: Illegal pointer conversion in operator =";
         if (!tmp) throw new PtrCastException<Z,X>(r);
         release();
         ptr=tmp;
         //count=new size_type(1);
      }
      return *this;
   }

   RCPtr& operator= (X *r)
   {
      if (ptr != r)
      {
         release();
         ptr = r;
         //count=new size_type(1);
      }
      return *this;
   }

#ifdef RT_DEBUG
   X& operator* () const {if (ptr) return *ptr; else throw new PtrException("dereferencing NULL pointer in operator*");}
   X* operator->() const {if (ptr) return  ptr; else throw new PtrException("dereferencing NULL pointer in operator->");}
#else
   X& operator* () const {return *ptr; }
   X* operator->() const {return  ptr; }
#endif
   X* get () const { return ptr;}

   bool unique () const
   {
      return (ptr && ptr->unique());
   }

   X *detach () 
   {
      if (ptr)
      {
         if (ptr->unique()) 
         {
            X *tmp = ptr;
            ptr = 0;
            return tmp;
         } else {
            throw new PtrException("Error: trying to detach a non-unique pointer in rc_ptrs.h");
         }
      } else {
         throw new PtrException("Error: trying to detach a NULL pointer in rc_ptrs.h");
      }
   }

protected:
   void release()
   {
      if (ptr)
	 ptr->unref();
      //else
      // cerr << "crisse de grosse erreur\n";
      ptr = 0;
   }

   void acquire()
   {
      if (ptr)
	 ptr->ref();
      //else
      // cerr << "crisse de vraiment grosse erreur\n";
   }

#ifndef BROKEN_TEMPLATES
   template <class Z>
   friend class RCPtr;
#endif
};




}//end namespace FD

#endif

