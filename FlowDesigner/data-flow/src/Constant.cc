// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau

#include "Constant.h"

using namespace std;

DECLARE_NODE(Constant)
/*Node
 *
 * @name Constant
 * @category General
 * @description Defines a constant in terms of type and value. The different types are: int, float, bool, string, objects and subnet_param.
 *
 * @output_name VALUE
 * @output_description The value (parameter)
 *
 * @parameter_name VALUE
 * @parameter_description The value
 *
END*/


Constant::Constant(string nodeName, ParameterSet params) 
   : Node(nodeName, params) 
   //, value (parameters.get("VALUE"))
{
   outputID = addOutput("VALUE");
   //value = parameters.get("VALUE");
}

void Constant::initialize()
{
  value = parameters.get("VALUE");
  this->Node::initialize();
}

void Constant::reset()
{
  this->Node::reset();
}

ObjectRef Constant::getOutput(int output_id, int count)
{
  if (output_id==outputID) return value;
  else throw new NodeException (this, "Constant: Unknown output id", __FILE__, __LINE__);
}
