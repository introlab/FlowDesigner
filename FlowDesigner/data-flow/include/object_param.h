// Copyright (C) 2001 Jean-Marc Valin

#ifndef OBJECT_PARAM_H
#define OBJECT_PARAM_H

#include <string>
#include <vector>
#include "ObjectRef.h"
#include "ParameterSet.h"

class ObjectParam {
public:

   static vector<string> allTypes(bool allowSubnetParam=true);
   
   static ObjectRef stringParam(string type, string value, ParameterSet &param);
   
};

#endif
