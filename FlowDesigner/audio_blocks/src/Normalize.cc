// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

using namespace std;

class Normalize;

DECLARE_NODE(Normalize)
/*Node
 *
 * @name Normalize
 * @category DSP:Base
 * @description Normalizes a vector by dividing each element by the sum of all components
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input vector
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Normalized vector
END*/


class Normalize : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   Normalize(string nodeName, ParameterSet params)
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

      float sum=0;
      for (int i=0;i<inputLength;i++)
	 sum += in[i];
      sum = 1.0/sum;

      for (int i=0;i<inputLength;i++)
      {
         output[i]=sum*in[i];
      }
      
   }

};
