// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "operators.h"

class Mul;

DECLARE_NODE(Mul)
/*Node
 *
 * @name Mul
 * @category Operator
 * @description Multiplication between two values, vectors, objects (operator* must be defined)
 *
 * @input_name INPUT1
 * @input_description The first operand
 *
 * @input_name INPUT2
 * @input_description The second operand 
 *
 * @output_name OUTPUT
 * @output_description The result of INPUT1 * INPUT2
 *
END*/


class Mul : public BufferedNode {
   
   int input1ID;
   int input2ID;
   int outputID;

public:
   Mul(string nodeName, ParameterSet params)
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

      out[count] = inputValue * input2Value;
   }

};
