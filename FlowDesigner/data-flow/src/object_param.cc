// Copyright (C) 2001 Jean-Marc Valin


#include "object_param.h"
#include "ParameterSet.h"
#include <string>
#include <sstream>
#include "ObjectParser.h"

vector<string> ObjectParam::allTypes(bool allowSubnetParam)
{
   static vector<string> types;
   types.insert(types.end(), "int");
   types.insert(types.end(), "float");
   types.insert(types.end(), "string");
   types.insert(types.end(), "bool");
   types.insert(types.end(), "object");
      //types.insert(types.end(), "object");
   if (allowSubnetParam)
      types.insert(types.end(), "subnet_param");
   return types;
}

ObjectRef ObjectParam::stringParam(string type, string value, ParameterSet &param)
{
   if (value == "")
      return Object::nilObject;
   //ObjectRef value;
   if (type == "int")
   {
      int val = atoi (value.c_str());
      return ObjectRef(new Int(val));
   }
   else if (type == "bool")
   {
      if (value == "true" || value == "TRUE")
	 return ObjectRef (new Bool(true));
      else if (value == "false" || value == "FALSE")
	 return ObjectRef(new Bool(false));
      else
      {
	 cerr << "value: " << value << endl;
	 throw new GeneralException("Bool value isn't either true or false... make up your mind", 
				    __FILE__, __LINE__);
      }
   }
   else if (type == "float")
   {
      float val = atof (value.c_str());
      return ObjectRef(new Float(val)); 
   } 
   else if (type == "string")
   {
      return ObjectRef(new String(value));
   } 
   else if (type == "object")
   {
      istringstream obj(value);
      ObjectRef val;
      obj >> val;
      return val;
   } 
   else if (type == "subnet_param")
   {
      if (param.exist(value))
	 return param.get(value);
      else
	 return Object::nilObject;
   }
   else {
      throw new GeneralException("Unknown parameter type: \"" + type + "\"", __FILE__, __LINE__);
   }
}
