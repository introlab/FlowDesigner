// Copyright (C) 2001 Jean-Marc Valin

#ifndef OBJECT_PARAM_H
#define OBJECT_PARAM_H

#ifdef _MSC_VER
#pragma warning (disable: 4786)
#endif

#include "ParameterSet.h"
#include "ObjectRef.h"
#include <string>
#include <vector>

class ObjectParam {
public:

   static const vector<string> &allTypes(bool allowSubnetParam=true);
   
   static ObjectRef stringParam(string type, string value, ParameterSet &param);
   
};

#endif
