// Copyright (C) 2001 Jean-Marc Valin


#include "object_param.h"
#include "ParameterSet.h"
#include <string>
#include <sstream>
#include "ObjectParser.h"

const vector<string> &ObjectParam::allTypes(bool allowSubnetParam)
{
   static vector<string> types;
   static vector<string> Stypes;
   static bool init=false;
   if (!init)
   {
      types.push_back("int");
      types.push_back("float");
      types.push_back("string");
      types.push_back("bool");
      types.push_back("object");

      Stypes.push_back("int");
      Stypes.push_back("float");
      Stypes.push_back("string");
      Stypes.push_back("bool");
      Stypes.push_back("object");
      Stypes.push_back("subnet_param");
      init = true;
   }
   if (allowSubnetParam)
      return Stypes;
   else
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
   else if (type == "expr")
   {
      throw new GeneralException("Expressions not supported yet in params", __FILE__, __LINE__);
      /*istringstream obj(value);
      ObjectRef val;
      obj >> val;
      return val;*/
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
