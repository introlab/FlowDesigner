// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "vmethod.h"

class VMethod;

DECLARE_NODE(VMethod)
/*Node
 *
 * @name VMethod
 * @category General
 * @description Applies a certain method on an object
 *
 * @input_name INPUT
 * @input_type any
 * @input_description Object (this)
 *
 * @output_name OUTPUT
 * @output_type any
 * @output_description Return value of the method
 *
 * @parameter_name METHOD
 * @parameter_type string
 * @parameter_description The name of the method to call
 *
END*/


class VMethod : public BufferedNode {
   
   int inputID;
   int outputID;
   string methodName;
   int methID;

public:
   VMethod(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      methodName = object_cast<String> (parameters.get("METHOD"));
      methID = vmethod()->lookup(methodName);
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);


      out[count] = vmethod()->call(methID, inputValue);
   }

      
};
