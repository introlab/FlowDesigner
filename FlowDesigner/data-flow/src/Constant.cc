// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau

#include "Constant.h"

//@require core

DECLARE_NODE(Constant)
/*Node
 *
 * @name Constant
 * @category General
 * @description Defines a constant
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

void Constant::specificInitialize()
{
   value = parameters.get("VALUE");
   this->Node::specificInitialize();
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
