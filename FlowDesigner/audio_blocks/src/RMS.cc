// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>

class RMS;

DECLARE_NODE(RMS)
/*Node
 *
 * @name RMS
 * @category DSP:Misc
 * @description Root mean squared (RMS) value of a signal
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description The input signal
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description The RMS value
 *
END*/


class RMS : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   RMS(string nodeName, ParameterSet params)
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

      //Vector<float> &output = *Vector<float>::alloc(1);
      //out[count] = &output;

      double energy=0;
      for (int i=0;i<inputLength;i++)
      {
	 energy+=in[i]*in[i];
      }

      out[count] = Float::alloc(energy/inputLength);
      //output[0]=sqrt(energy/inputLength);
   }

};
