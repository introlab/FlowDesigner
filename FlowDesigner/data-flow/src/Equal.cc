// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "operators.h"

using namespace std;
using namespace FD;

class Equal;

DECLARE_NODE(Equal)
/*Node
 *
 * @name Equal
 * @category Operator
 * @description Returns true if two input values are equal, false otherwise
 *
 * @input_name INPUT1
 * @input_description First value
 *
 * @input_name INPUT2
 * @input_description Second value
 *
 * @output_name OUTPUT
 * @output_type bool
 * @output_description True or false
 *
END*/


class Equal : public BufferedNode {
   
   int input1ID;
   int input2ID;
   int outputID;

public:
   Equal(string nodeName, ParameterSet params)
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
      out[count] = inputValue == input2Value;
   }

};
