// Copyright (C) 2001 LocusDialog 

#include "BufferedNode.h"
#include "Vector.h"
#include <math.h>

using namespace std;

namespace FD {

class Floor;

DECLARE_NODE(Floor)
/*Node
 *
 * @name Floor
 * @category DSP:Base
 * @description Floors vector values
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input vector
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Output vector (after flooring)
 *
 * @parameter_name THRESH
 * @parameter_type float
 * @parameter_description Threshold
 *
END*/


class Floor : public BufferedNode {
   
   int inputID;
   int outputID;
   float thresh;

public:
   Floor(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      thresh = dereference_cast<float> (parameters.get("THRESH"));
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;

      for (int i=0; i<inputLength;i++)
	 output[i] = max(0.0f, in[i]);
   }

      
};
}//namespace FD
