// Copyright (C) 1999 Jean-Marc Valin

#include "Object.h"
#include "ObjectParser.h"
#include "Exception.h"

//@implements core

map<string, _ObjectFactory*>& Object::ObjectFactoryDictionary()
{
   static map<string, _ObjectFactory*> dict;
   return dict;
}

//map<const type_info *, _ObjectFactory*>& Object::TypeidDictionary()
TypeMap<_ObjectFactory*>& Object::TypeidDictionary()
{
   static TypeMap<_ObjectFactory*> dict;
   return dict;
}

string Object::className() const
{
   TypeMap<_ObjectFactory*> &m = TypeidDictionary();
   TypeMap<_ObjectFactory*>::iterator found = m.find(&typeid(*this));
   if (found != m.end())
      return found->second->getName();
   else
      return "unknown";
}

ObjectRef Object::newObject(const string &objType)
{
   if (ObjectFactoryDictionary().find(objType) != ObjectFactoryDictionary().end())
   {
      return ObjectFactoryDictionary()[objType]->create();
   } else
   {
      throw new GeneralException ("ObjectRef error: unknown type " + objType, __FILE__, __LINE__);
   }
}

void Object::serialize(ostream &out) const
{
   throw new GeneralException("Object doesn't know how to serialize itself", __FILE__, __LINE__);
}

void Object::unserialize(istream &in)
{
   throw new GeneralException("Object doesn't know how to unserialize itself", __FILE__, __LINE__);
}

void Object::doesNotUnderstand(string method)
{
   throw new GeneralException(string(typeid (*this).name()) + ": Does Not Understand method " + method, __FILE__, __LINE__);
}


ObjectRef nilObject = ObjectRef(new NilObject);
