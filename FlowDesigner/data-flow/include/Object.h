// Copyright (C) 1999 Jean-Marc Valin

#ifndef _OBJECT_H_
#define _OBJECT_H_

#include "rc_ptrs.h"
#include <string>
#include <map>
#include "BaseException.h"
#include <typeinfo>

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

   /**Returns the name of the class of the Object*/
   virtual string className() const;
   
   template<class T>
   static string GetClassName();

   static const ObjectRef nilObject;
   static const ObjectRef before_beginningObject;
   static const ObjectRef past_endObject;
   
   /**Creates an instance of an object by class name*/
   static ObjectRef newObject(const string &objType);

   /**Registers the object name*/
   template<class T>
   static int addObjectType(const string &objType, _ObjectFactory *factory)
   {
      ObjectFactoryDictionary()[objType] = factory;
      TypeidDictionary()[&typeid(T)] = factory;
      return 0;
   }
private:
   static map<string, _ObjectFactory*>& ObjectFactoryDictionary();
   static map<const type_info *, _ObjectFactory*>& TypeidDictionary();
};





class _ObjectFactory {
   string typeName;
public:
   _ObjectFactory(const string &_name) : typeName(_name) {}
   virtual ObjectRef create() = 0;
   const string &getName() {return typeName;}
};

template <class T>
class ObjectFactory : public _ObjectFactory {
public:
   ObjectFactory(const string &_name) : _ObjectFactory(_name) {}
   virtual ObjectRef create() {return ObjectRef(new T);}
};




template<class T>
string Object::GetClassName()
{
   map<const type_info *, _ObjectFactory*> &m = TypeidDictionary();
   map<const type_info *, _ObjectFactory*>::iterator found = m.find(&typeid(T));
   if (found != m.end())
      return found->second->getName();
   else
   {
      //throw GeneralException ("Object::GetClassName() failed, object type is not registered",
      //		      __FILE__, __LINE__);
      //static const string unknown("unknown");
      return "unknown";
   }
}




#define DECLARE_TYPE(type) static int dummy_init_for ## type = \
               Object::addObjectType<type > (# type, new ObjectFactory<type> (#type));

#define DECLARE_TYPE2(type, dummyID) static int dummy_init_for ## dummyID = \
               Object::addObjectType<type > (# type, new ObjectFactory<type> (#type));

#define DECLARE_TYPE3(str, type, dummyID) static int dummy_init_for ## dummyID = \
               Object::addObjectType<type > (str, new ObjectFactory<type> (str));

#endif
