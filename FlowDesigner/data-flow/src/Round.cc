// Copyright (C) 2001 InfoSpace Speech Solutions
// Author: Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>

class Round;

DECLARE_NODE(Round)
/*Node
 *
 * @name Round
 * @category Vector
 * @description Rounds a float values to the nearest integer
 *
 * @input_name INPUT
 * @input_type float
 * @input_description The input float 
 *
 * @output_name OUTPUT
 * @output_type Int
 * @output_description Nearest integer
 *
END*/


class Round : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   Round(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);
      out[count] = ObjectRef(Int::alloc( int(rint(dereference_cast<float> (inputValue))) ));

      
   }

      
NO_ORDER_NODE_SPEEDUP(Round)
};
