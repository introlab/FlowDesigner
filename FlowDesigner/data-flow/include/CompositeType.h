// Copyright (C) 2001 Jean-Marc Valin

#ifndef COMPOSITE_TYPE_H
#define COMPOSITE_TYPE_H

#include "Object.h"
#include <map>

/**Allows user-defined types (analog to structs in C/C++)*/
class CompositeType : public Object {
   typedef map<string, ObjectRef> map_type;
   map_type fields;
public:
   CompositeType() {}
   void printOn(ostream &out) const;
   void readFrom(istream &in);
   void addField(const string &name, ObjectRef obj) {fields[name]=obj;}
   ObjectRef get(const string &name) const
   {
      map_type::const_iterator it = fields.find(name);
      if (it==fields.end())
         throw new GeneralException(string("Unknown field: ") + name, __FILE__, __LINE__);
      return it->second;
   }
};

#endif
