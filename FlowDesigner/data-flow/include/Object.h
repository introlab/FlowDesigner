// Copyright (C) 1999 Jean-Marc Valin

#ifndef _OBJECT_H_
#define _OBJECT_H_

#include "rc_ptrs.h"
#include <string>
#include <map>
#include "BaseException.h"

class Object;
/** Smart pointer to Object called ObjectRef
    @author Jean-Marc Valin
    @version 1.0
 */
typedef RCPtr<Object> ObjectRef;

class _ObjectFactory;

/** Our Object base class. Everything in the network must have Object as 
    base class.
    @author Dominic Letourneau & Jean-Marc Valin
*/
class Object {
  protected:
   
   int ref_count;
   
public:

   /**The status of an object*/
   enum ObjectStatus {valid=0, before_beginning, erased, past_end, compute_error, wait, nil};

   /**default constructor*/
   Object() :status(valid){ref_count=1;}

   /**constructor with a status*/
   Object(ObjectStatus st) :status(st){ref_count=1;}

   /**destructor*/
   virtual ~Object() { }

   /**The current status*/
   ObjectStatus status;

   /**Notify the object we're adding a reference*/
   void ref() 
   {
      ref_count++;
   }

   /**Notify the object we're removing a reference (might destroy the object)*/
   void unref()
   {
      if (--ref_count==0)
      {
	 destroy();
      }
   }

   /**Returns the number of references*/
   int getCount () {return ref_count;}

   /**Causes the object to be destroyed, it might be redefined for an object pool*/
   virtual void destroy()
   {
      delete this;
   }

   /**Serialize (binary) the object to a stream*/
   virtual void serialize(ostream &out) const;

   /**Unserialize (binary) the object from a stream*/
   virtual void unserialize(istream &in);
   
   /**How to handle an ununderstood method (VMethod)*/
   virtual void doesNotUnderstand(string method);

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
      throw new GeneralException("Trying to read undefined Object", __FILE__, __LINE__);
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

class _ObjectFactory
{
public:
   virtual ObjectRef create() = 0;
};

template <class T>
class ObjectFactory : public _ObjectFactory {
public:
   virtual ObjectRef create() {return ObjectRef(new T);}
};


#define DECLARE_TYPE(type) static int dummy_init_for ## type = \
               Object::addObjectType (# type, new ObjectFactory<type>);

#define DECLARE_TYPE2(type, dummyID) static int dummy_init_for ## dummyID = \
               Object::addObjectType (# type, new ObjectFactory<type>);

#endif
