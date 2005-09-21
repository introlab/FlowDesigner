// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "vmethod.h"

using namespace std;

namespace FD {

class VMethod;

DECLARE_NODE(VMethod)
/*Node
 *
 * @name VMethod
 * @category General
 * @description Applies a certain method on an object int or float. The name of the method to call can be: log, exp, sin or cos.
 *
 * @input_name INPUT
 * @input_type int or float
 * @input_description Object on wich the method will be applied
 *
 * @output_name OUTPUT
 * @output_type int or float
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

}//namespace FD
