// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau
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

#ifndef OBJECTREF_H
#define OBJECTREF_H


#include "Object.h"
#include "net_types.h"
#include "Exception.h"
#include <typeinfo>


class GenericCastException : public BaseException{
public:
   virtual void print(ostream &out = cerr)=0;
};

/** 
    The CastException occurs when we are unable to cast an ObjectRef.
    @author Jean-Marc Valin.
    @version 1.0
 */
template <class T>
class CastException : public GenericCastException {
public:
   /**The constructur that takes an error message*/
   CastException(string _type) : type(_type) 
   {} 
  
   /**The print method*/
   virtual void print(ostream &out = cerr) 
   {
      out << "Cast error: Trying to cast ObjectRef (" << type << ") into " << typeid(T).name() << endl;
   }

protected:
   /**The error message*/
   string type;
};

/**The object pointer cast from ObjectRef*/
template <class T>
T object_ptr_cast (const ObjectRef &ref)
{
   T tmp = dynamic_cast<T>(&(*ref));
   if (!tmp) 
      throw CastException<T> (typeid ((*ref)).name());
   return tmp;
}

/**The object cast from ObjectRef*/
template <class T>
T &object_cast (const ObjectRef &ref)
{
   T *tmp = dynamic_cast<T *>(&(*ref));
   if (!tmp) 
      throw CastException<T> (typeid ((*ref)).name());
   return *tmp;
}

/**The type cast from ObjectRef*/
template <class T>
T &dereference_cast (const ObjectRef &ref)
{
   GenericType<T> *tmp = (dynamic_cast<GenericType<T> * >(&(*ref)));
   if (!tmp) 
      throw CastException<T> (typeid ((*ref)).name());
   return tmp->val();
}



#endif
