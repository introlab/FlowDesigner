// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "operators.h"

using namespace std;

class Div;

DECLARE_NODE(Div)
/*Node
 *
 * @name Div
 * @category Operator
 * @description Divides a numerator by a denominator
 *
 * @input_name NUM
 * @input_description The numerator
 *
 * @input_name DEN
 * @input_description The denominator
 *
 * @output_name OUTPUT
 * @output_description The result of the division
 *
END*/


class Div : public BufferedNode {
   
   int input1ID;
   int input2ID;
   int outputID;

public:
   Div(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      input1ID = addInput("NUM");
      input2ID = addInput("DEN");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(input1ID, count);

      ObjectRef input2Value = getInput(input2ID, count);

      out[count] = inputValue / input2Value;
   }

};
