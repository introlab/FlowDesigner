// Copyright (C) 1999 Jean-Marc Valin

#ifndef _OBJECT_H_
#define _OBJECT_H_

#include "typemap.h"
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

   /**default constructor*/
   Object() : ref_count(1) {}

   /**destructor*/
   virtual ~Object() { }

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
   virtual void printOn(ostream &out=cout) const = 0;

   /**Is it a nil Object*/
   virtual bool isNil() const {return false;}
   
   /**Prints an object in a more "user-friendly" format*/
   virtual void prettyPrint(ostream &out=cout) const
   {
      printOn(out);
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
      
   /**Creates an instance of an object by class name*/
   static ObjectRef newObject(const string &objType);

#ifndef BROKEN_TEMPLATES /*Workaround for a compiler crash */

   /**Registers the object name*/
   template<class T>
   static int addObjectType(const string &objType, _ObjectFactory *factory)
   {
      ObjectFactoryDictionary()[objType] = factory;
      TypeidDictionary()[&typeid(T)] = factory;
      return 0;
   }

/*Because of f*ck*ng MSVC++*/
//private:

#endif

   static map<string, _ObjectFactory*>& ObjectFactoryDictionary();
   static TypeMap<_ObjectFactory*>& TypeidDictionary();
   //static map<const type_info *, _ObjectFactory*>& TypeidDictionary();
};


#ifdef BROKEN_TEMPLATES /*Workaround for a compiler crash */

/**Registers the object name*/
template<class T>
static int ObjectaddObjectType(const string &objType, _ObjectFactory *factory)
{
   Object::ObjectFactoryDictionary()[objType] = factory;
   Object::TypeidDictionary()[&typeid(T)] = factory;
   return 0;
}

#endif


class _ObjectFactory {
   string typeName;
public:
   _ObjectFactory(const string &_name) : typeName(_name) {}
   virtual ~_ObjectFactory() {}
   virtual ObjectRef create() = 0;
   const string &getName() {return typeName;}
};

template <class T>
class ObjectFactory : public _ObjectFactory {
public:
   ObjectFactory(const string &_name) : _ObjectFactory(_name) {}
   virtual ObjectRef create() {return ObjectRef(new T);}
};



/* This used to be Object::GetClassName<T>() but it changed because of stupid MSVC++ bugs*/
template<class T>
string ObjectGetClassName()
{
   static TypeMap<_ObjectFactory*> &m = Object::TypeidDictionary();
   static TypeMap<_ObjectFactory*>::iterator found = m.find(&typeid(T));
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


#define UNIQUE_STRING(line) dummy_init_for ## line

#ifndef BROKEN_TEMPLATES /*Workaround for a compiler crash */

#define DECLARE_TYPE2A(type, dummyID) static int UNIQUE_STRING(dummyID) = \
               Object::addObjectType<type > (# type, new ObjectFactory<type> (#type));

#define DECLARE_TYPE3A(str, type, dummyID) static int UNIQUE_STRING(dummyID) = \
               Object::addObjectType<type > (str, new ObjectFactory<type> (str));

#define DECLARE_TYPE(type) DECLARE_TYPE2A(type, __LINE__)
#define DECLARE_TYPE2(str, type) DECLARE_TYPE3A(str, type, __LINE__)
#else


#define DECLARE_TYPE(type) static int dummy_init_for ## type = \
               ObjectaddObjectType<type > (# type, new ObjectFactory<type > (#type));

#define DECLARE_TYPE2(type, dummyID) static int UNIQUE_STRING(dummy_init_for, dummyID) = \
               ObjectaddObjectType<type > (# type, new ObjectFactory<type > (#type));

#define DECLARE_TYPE3(str, type, dummyID) static int dummy_init_for ## dummyID = \
               ObjectaddObjectType<type > (str, new ObjectFactory<type > (str));


#endif /*BROKEN_TEMPLATES*/



class NilObject : public Object {
public:
   virtual void printOn(ostream &out=cout) const
   {
      out << "<NilObject >";
   }

   /**Is it a nil Object*/
   virtual bool isNil() const {return true;}
};

extern ObjectRef nilObject;

#endif
