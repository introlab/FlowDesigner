// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

class Sum;

DECLARE_NODE(Sum)
/*Node
 *
 * @name Sum
 * @category Vector
 * @description Sum of all the elements of a vector
 *
 * @input_name INPUT
 * @input_description The input vector 
 * @input_type Vector<float>
 *
 * @output_name OUTPUT
 * @output_description The sum
 * @output_type float
 *
END*/


class Sum : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   Sum(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();      

      float val = 0;
      for (int i=0;i<inputLength;i++)
      {
         val+=in[i];
      }
      
      out[count] = new Float (val);

      
   }

      
NO_ORDER_NODE_SPEEDUP(Sum)
};
