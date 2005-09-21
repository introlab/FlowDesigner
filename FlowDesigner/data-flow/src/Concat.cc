// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "operators.h"

using namespace std;
namespace FD {

class Concat;

DECLARE_NODE(Concat)
/*Node
 *
 * @name Concat
 * @category Operator
 * @description Concatenates two input values and returns the result
 *
 * @input_name INPUT1
 * @input_description First value
 *
 * @input_name INPUT2
 * @input_description Second value
 *
 * @output_name OUTPUT
 * @output_description Result of the concatenation
 *
END*/


class Concat : public BufferedNode {
   
   int input1ID;
   int input2ID;
   int outputID;

public:
   Concat(string nodeName, ParameterSet params)
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
      out[count] = concat(inputValue, input2Value);
   }

};

}//namespace FD
