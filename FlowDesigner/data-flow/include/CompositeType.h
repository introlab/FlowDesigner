// Copyright (C) 2001 Jean-Marc Valin

#ifndef COMPOSITE_TYPE_H
#define COMPOSITE_TYPE_H

#include "Object.h"
#include <map>

class CompositeType : public Object {
   map<string, ObjectRef> fields;
public:
   CompositeType();
   void printOn(ostream &out) const;
   void readFrom(istream &in);
   void serialize(ostream &out) const;
   void unserialize(istream &in);
   void addField(const string &name, ObjectRef obj);
   
};

#endif
