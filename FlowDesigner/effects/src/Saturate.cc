// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>

using namespace std;
using namespace FD;

class Saturate;

DECLARE_NODE(Saturate)
/*Node
 *
 * @name Saturate
 * @category DSP:Effects
 * @description Saturation (distortion) effect
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name SATURATION
 * @parameter_description No description available
 *
 * @parameter_name THRESHOLD
 * @parameter_description No description available
 *
END*/


class Saturate : public BufferedNode {
   
   int inputID;
   int outputID;
   enum Type {HARD, TANH, ATAN, SOFT4};

   float threshold;
   Type saturation;

public:
   Saturate(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");

      threshold = dereference_cast<float> (parameters.get("THRESHOLD"));
      
      if (parameters.exist("SATURATION"))
      {
	 String satur = object_cast<String> (parameters.get("SATURATION"));
	 if (satur == "hard")
	    saturation = HARD;
	 else if (satur == "tanh")
	    saturation = TANH;
	 else if (satur == "atan")
	    saturation = ATAN;
	 else if (satur == "soft4")
	    saturation = SOFT4;
	 else saturation = HARD;
      } else saturation = HARD;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;

      switch (saturation)
      {
	 case HARD:
	    for (int i=0;i<inputLength;i++)
	    {
	       output[i] = in[i];
	       if (output[i] < -threshold)
		  output[i] = -threshold;
	       else if (output[i] > threshold)
		  output[i] = threshold;
	    }
	    break;
	 case ATAN:
	    for (int i=0;i<inputLength;i++)
	    {
	       output[i] = ((2/M_PI)*threshold) * atan(in[i]/threshold);
	    }
	    break;
	 case TANH:
	    for (int i=0;i<inputLength;i++)
	    {
	       output[i] = threshold * tanh(in[i]/threshold);
	    }
	    break;
      }
   }

};
