// Copyright (C) 2001 Jean-Marc Valin & Dominic Letourneau

#include "BufferedNode.h"
#include "operators.h"

class Max;
DECLARE_NODE(Max)
/*Node
 *
 * @name Max
 * @category Operator
 * @description The maximum value
 *
 * @input_name INPUT1
 * @input_description First value
 *
 * @input_name INPUT2
 * @input_description Second value
 *
 * @output_name OUTPUT
 * @output_description The maximum value between INPUT1 and INPUT2
 *
END*/


class Max : public BufferedNode {
protected:
   ///The ID of the 'output' output
   int outputID;

   ///The ID of the 'input1' input
   int input1ID;

   ///The ID of the 'input2' input
   int input2ID;

public:
   ///Constructor, takes the name of the node and a set of parameters
   Max(string nodeName, ParameterSet params) 
     : BufferedNode(nodeName, params)
   {
      input1ID = addInput ("INPUT1");
      input2ID = addInput ("INPUT2");
      outputID = addOutput ("OUTPUT"); 
   }
   


  void calculate(int output_id, int count, Buffer &out) {

     ObjectRef InputValue1 = getInput(input1ID,count);     
     ObjectRef InputValue2 = getInput(input2ID,count);

     out[count] = max(InputValue1, InputValue2);

   }
   
};

