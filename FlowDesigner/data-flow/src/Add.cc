// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "operators.h"

class Add;

DECLARE_NODE(Add)
/*Node
 *
 * @name Add
 * @category Operator
 * @description Adds two input values and returns the result
 *
 * @input_name INPUT1
 * @input_description First value
 *
 * @input_name INPUT2
 * @input_description Second value
 *
 * @output_name OUTPUT
 * @output_description Result of the addition
 *
END*/


class Add : public BufferedNode {
   
   int input1ID;
   int input2ID;
   int outputID;

public:
   Add(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      input1ID = addInput("INPUT1");
      input2ID = addInput("INPUT2");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(input1ID, count);
      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
         return;
      }

      ObjectRef input2Value = getInput(input2ID, count);
      if (input2Value->status != Object::valid)
      {
	 out[count] = input2Value;
	 return;
      }
      out[count] = inputValue + input2Value;
   }

};
