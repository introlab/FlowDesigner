// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

class Abs;

DECLARE_NODE(Abs)
/*Node
 *
 * @name Abs
 * @category DSP:Base
 * @description Computes the absolute value of each element of a vector
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input vector
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Output vector
 *
END*/


class Abs : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   Abs(string nodeName, ParameterSet params)
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

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;

      for (int i=0;i<inputLength;i++)
      {
         output[i]=in[i];
	 if (output[i] < 0)
	    output[i] = -output[i];
      }
   }

};
