// Copyright (C) 2001 Jean-Marc Valin & Dominic Letourneau

#include "BufferedNode.h"
#include "operators.h"

using namespace std;
using namespace FD;

class Smaller;
DECLARE_NODE(Smaller)
/*Node
 *
 * @name Smaller
 * @category Logic
 * @description Verifies if INPUT1 is smaller than INPUT2
 *
 * @input_name INPUT1
 * @input_description The first operand
 *
 * @input_name INPUT2
 * @input_description The second operand
 *
 * @output_name OUTPUT
 * @output_description Boolean output
 *
END*/


class Smaller : public BufferedNode {
protected:
   ///The ID of the 'output' output
   int outputID;

   ///The ID of the 'input1' input
   int input1ID;

   ///The ID of the 'input2' input
   int input2ID;


public:
   ///Constructor, takes the name of the node and a set of parameters
   Smaller(string nodeName, ParameterSet params) 
     : BufferedNode(nodeName, params)
   {
      input1ID = addInput ("INPUT1");
      input2ID = addInput ("INPUT2");
      outputID = addOutput ("OUTPUT");


   }
   
  void calculate(int output_id, int count, Buffer &out) {

     ObjectRef InputValue1 = getInput(input1ID,count);
     ObjectRef InputValue2 = getInput(input2ID,count);

     out[count] = InputValue1 < InputValue2;

   }
   
};

