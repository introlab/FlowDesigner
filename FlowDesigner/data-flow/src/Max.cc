// Copyright (C) 2001 Jean-Marc Valin & Dominic Letourneau

#include "BufferedNode.h"
#include "operators.h"

class MaxValue;
DECLARE_NODE(MaxValue)
/*Node
 *
 * @name MaxValue
 * @category Math
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


class MaxValue : public BufferedNode {
protected:
   ///The ID of the 'output' output
   int outputID;

   ///The ID of the 'input1' input
   int input1ID;

   ///The ID of the 'input2' input
   int input2ID;

public:
   ///Constructor, takes the name of the node and a set of parameters
   MaxValue(string nodeName, ParameterSet params) 
     : BufferedNode(nodeName, params)
   {
      input1ID = addInput ("INPUT1");
      input2ID = addInput ("INPUT2");
      outputID = addOutput ("OUTPUT"); 
   }
   


  void calculate(int output_id, int count, Buffer &out) {

     ObjectRef InputValue1 = getInput(input1ID,count);
     
     if (InputValue1->status != Object::valid) {
       out[count] = InputValue1;
       return;
     }
     
     ObjectRef InputValue2 = getInput(input2ID,count);

     if (InputValue2->status != Object::valid) {
       out[count] = InputValue2;
       return;
     }

     
     if (dereference_cast<bool>(InputValue1 > InputValue2)) {
       out[count] = InputValue1;
     }
     else {
       out[count] = InputValue2;
     }

   }
   
};

