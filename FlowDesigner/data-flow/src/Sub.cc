// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "operators.h"

class Sub;

DECLARE_NODE(Sub)
/*Node
 *
 * @name Sub
 * @category Operator
 * @description Subtracts two values, Vectors, Objects
 *
 * @input_name INPUT1
 * @input_description The value to subtract from
 *
 * @input_name INPUT2
 * @input_description The subtracted value
 *
 * @output_name OUTPUT
 * @output_description The result of the subtraction
 *
END*/


class Sub : public BufferedNode {
   
   int input1ID;
   int input2ID;
   int outputID;

public:
   Sub(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      input1ID = addInput("INPUT1");
      input2ID = addInput("INPUT2");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(input1ID, count);

      ObjectRef input2Value = getInput(input2ID, count);

      out[count] = inputValue - input2Value;
   }

NO_ORDER_NODE_SPEEDUP(Sub)
};
