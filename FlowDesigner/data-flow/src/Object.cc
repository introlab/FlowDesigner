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

void Object::unserialize(ostream &out)
{
   throw new GeneralException("Object doesn't know how to unserialize itself", __FILE__, __LINE__);
}

void Object::doesNotUnderstand(string method)
{
   throw new GeneralException(string(typeid (*this).name()) + ": Does Not Understand method " + method, __FILE__, __LINE__);
}
