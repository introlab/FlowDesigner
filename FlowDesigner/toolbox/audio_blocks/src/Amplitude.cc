// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>

using namespace std;

namespace FD {

class Amplitude;

DECLARE_NODE(Amplitude)
/*Node
 *
 * @name Amplitude
 * @category ZDeprecated
 * @description Deprecated, use RMS instead
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
END*/


class Amplitude : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   Amplitude(string nodeName, ParameterSet params)
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

      Vector<float> &output = *Vector<float>::alloc(1);
      out[count] = &output;

      float energy=0;
      for (int i=0;i<inputLength;i++)
      {
	 energy+=in[i]*in[i];
      }

      output[0]=sqrt(energy+.0001);
   }

};
}//namespace FD
