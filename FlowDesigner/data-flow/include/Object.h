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

#ifndef _OBJECT_H_
#define _OBJECT_H_

#include "rc_ptrs.h"
#include <string>
#include <map>
#include "Exception.h"

class Object;
/** Smart pointer to Object called ObjectRef
    @author Jean-Marc Valin
    @version 1.0
 */
typedef Ptr<Object> ObjectRef;

class _ObjectFactory;

/** Our Object base class. Everything in the network must have Object as 
    base class.
    @author Dominic Letourneau & Jean-Marc Valin
*/
class Object {

public:

   /**The status of an object*/
   enum ObjectStatus {valid=0, before_beginning, erased, past_end, compute_error, wait, nil};

   /**default constructor*/
   Object() :status(valid){}

   /**constructor with a status*/
   Object(ObjectStatus st) :status(st){}

   /**destructor*/
   virtual ~Object() { }

   /**The current status*/
   ObjectStatus status;

   /**raw (binary) output method*/
   virtual void rawWrite(ostream &out) const
   {
      throw GeneralException("Object doesn't know how to rawWrite itself", __FILE__, __LINE__);
   }

   /**Generic print function*/
   virtual void printOn(ostream &out=cout) const
   {
      if (typeid (*this) == typeid(Object))
         out << "<Object <status " << status << "> >" << endl;
      else 
         out << "<" << typeid (*this).name() << " <status " << status << "> >" << endl;
   }
   
   /**Generic read function*/
   virtual void readFrom(istream &in=cin)
   {
      throw GeneralException("Trying to read undefined Object", __FILE__, __LINE__);
   }

   /**Prints the object to a stream*/
   friend ostream &operator << (ostream &out, const Object& obj) 
   {
      obj.printOn (out);
      return out;
   }

   static const ObjectRef nilObject;
   static const ObjectRef before_beginningObject;
   static const ObjectRef past_endObject;
   
   static ObjectRef newObject(const string &objType);
   static int addObjectType(const string &objType, _ObjectFactory *factory);
private:
   static map<string, _ObjectFactory*>& ObjectFactoryDictionary();
};

#define DECLARE_TYPE(type) static int dummy = \
               Object::addObjectType (# type, new ObjectFactory<type>);

#endif
