// Copyright (C) 1999 Jean-Marc Valin

#ifndef _OBJECT_H_
#define _OBJECT_H_

#include "typemap.h"
#include "rc_ptrs.h"
#include <string>
#include <map>
#include "BaseException.h"
#include <typeinfo>
#include "multithread.h"

namespace FD {

class _ObjectFactory;

/** Our Object base class. Everything in the network must have Object as 
    base class.
    @author Dominic Letourneau & Jean-Marc Valin
*/
class Object
{
   protected:
   
      AtomicCounter ref_count;
   
   public:

      /**default constructor*/
      Object() : ref_count(1) {}

      /**destructor*/
      virtual ~Object() { }

      /**Notify the object we're adding a reference*/
      void ref() 
      {
         ref_count.inc();
      }

      /**Notify the object we're removing a reference (might destroy the object)*/
      void unref()
      {
         if (ref_count.dec()==0)
         {
	    destroy();
         }
      }

      /**Returns the number of references*/
      int unique () {return ref_count.unique();}

      /**Causes the object to be destroyed, it might be redefined for an object pool*/
      virtual void destroy()
      {
         delete this;
      }

      /**Serialize (binary) the object to a stream*/
      virtual void serialize(std::ostream &out) const;

      /**Unserialize (binary) the object from a stream*/
      virtual void unserialize(std::istream &in);
   
      /**How to handle an ununderstood method (VMethod)*/
      virtual void doesNotUnderstand(std::string method);

      /**Generic print function*/
      virtual void printOn(std::ostream &out=std::cout) const = 0;

      /**Is it a nil Object*/
      virtual bool isNil() const {return false;}
   
      /**Prints an object in a more "user-friendly" format*/
      virtual void prettyPrint(std::ostream &out=std::cout) const
      {
         printOn(out);
      }

      /**Generic read function*/
      virtual void readFrom(std::istream &in=std::cin)
      {
         throw new GeneralException("Trying to read undefined Object", __FILE__, __LINE__);
      }

      /**Prints the object to a stream*/
      friend std::ostream &operator << (std::ostream &out, const Object& obj) 
      {
         obj.printOn (out);
         return out;
      }

      /**Makes a (deep) copy of the object*/
      virtual ObjectRef clone()
      {
	 std::string message = "Method clone() not implemented for this object : ";
	 message += typeid(this).name();
         throw new GeneralException(message, __FILE__, __LINE__);
      }

      /**Returns the name of the class of the Object*/
      virtual std::string className() const;
      
      /**Creates an instance of an object by class name*/
      static ObjectRef newObject(const std::string &objType);

      /**Registers the object name*/
      template<class T>
      static int addObjectType(const std::string &objType, _ObjectFactory *factory)
      {
	 if (ObjectFactoryDictionary().find(objType) != ObjectFactoryDictionary().end())
	 {
		 std::string message = std::string("Duplicated object type found : ") 
			 + objType + std::string(", it not be inserted in the ObjectFactoryDictionary.");
		 throw new GeneralException(message,__FILE__,__LINE__);
		 return -1;
	 }
	 else
	 {
         	ObjectFactoryDictionary()[objType] = factory;
         	TypeidDictionary()[&typeid(T)] = factory;
         	return 0;
	 }
      }

      static std::map<std::string, _ObjectFactory*>& ObjectFactoryDictionary();
      static TypeMap<_ObjectFactory*>& TypeidDictionary();
      //static map<const type_info *, _ObjectFactory*>& TypeidDictionary();
};

class _ObjectFactory 
{
   std::string typeName;

   public:
      _ObjectFactory(const std::string &_name) : typeName(_name) {}
      virtual ~_ObjectFactory() {}
      virtual ObjectRef create() = 0;
      const std::string &getName() {return typeName;}
};

template <class T>
class ObjectFactory : public _ObjectFactory
{
   public:
      ObjectFactory(const std::string &_name) : _ObjectFactory(_name) {}
      virtual ObjectRef create() {return ObjectRef(new T);}
};


/* This used to be Object::GetClassName<T>() but it changed because of stupid MSVC++ bugs*/
template<class T>
std::string ObjectGetClassName()
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

#define DECLARE_TYPE2A(type, dummyID) static int UNIQUE_STRING(dummyID) = \
               Object::addObjectType<type > (# type, new ObjectFactory<type > (#type));

#define DECLARE_TYPE3A(str, type, dummyID) static int UNIQUE_STRING(dummyID) = \
               Object::addObjectType<type > (str, new ObjectFactory<type > (str));

#define DECLARE_TYPE(type) DECLARE_TYPE2A(type, __LINE__)
#define DECLARE_TYPE2(str, type) DECLARE_TYPE3A(str, type, __LINE__)


class NilObject : public Object 
{
   public:
      virtual void printOn(std::ostream &out=std::cout) const
      {
         out << "<NilObject >";
      }

      /**Is it a nil Object*/
      virtual bool isNil() const { return true; }

      virtual void readFrom(std::istream &in=std::cin)
      {
         char ch;
         in >> ch;
         if (ch != '>')
	    throw new GeneralException("Error reading NilObject: '>' expected", __FILE__, __LINE__); 
      }

};

extern ObjectRef nilObject;


//ObjectRef ObjectFactory<NilObject>::create() {return nilObject;}

}//namespace FD
#endif
