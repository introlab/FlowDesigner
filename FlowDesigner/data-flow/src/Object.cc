// Copyright (C) 1999 Jean-Marc Valin

#include "Object.h"
#include "ObjectParser.h"
#include "Exception.h"

const ObjectRef Object::nilObject = ObjectRef (new Object(Object::nil));
const ObjectRef Object::before_beginningObject = ObjectRef (new Object(Object::before_beginning));
const ObjectRef Object::past_endObject = ObjectRef (new Object(Object::past_end));

map<string, _ObjectFactory*>& Object::ObjectFactoryDictionary()
{
   static map<string, _ObjectFactory*> dict;
   return dict;
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

int Object::addObjectType(const string &objType, _ObjectFactory *factory)
{
   ObjectFactoryDictionary()[objType] = factory;
   return 0;
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
