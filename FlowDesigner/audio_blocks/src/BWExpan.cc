// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

class BWExpan;

DECLARE_NODE(BWExpan)
/*Node
 *
 * @name BWExpan
 * @category DSP:Adaptive
 * @description Performs bandwidth expansion on an LPC filter, that is, multiplying the radius of the poles by GAMMA
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Original LPC filter
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description New "bandwidth expanded" LPC filter
 *
 * @parameter_name GAMMA
 * @parameter_type float
 * @parameter_description Pole radius factor
 *
END*/


class BWExpan : public BufferedNode {
   
   int inputID;
   int outputID;
   vector<float> gains;
   float gamma;

public:
   BWExpan(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      , gains(1,1)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      gamma = dereference_cast<float> (parameters.get("GAMMA"));
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;

      
      for (int i=gains.size();i<inputLength;i++)
	 gains.push_back(float(gamma*gains[i-1]));
      
      for (int i=0;i<inputLength;i++)
      {
         output[i]=gains[i]*in[i];
      }
      
   }

      
};
