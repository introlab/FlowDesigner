// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

using namespace std;

class FFTFlip;

DECLARE_NODE(FFTFlip)
/*Node
 *
 * @name FFTFlip
 * @category DSP:Base
 * @description Flips a half-spectrum to produce a symetric spectrum
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Half spectrum (real)
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Symetric spectrum
 *
END*/


class FFTFlip : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   FFTFlip(string nodeName, ParameterSet params)
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

      Vector<float> &output = *Vector<float>::alloc(inputLength<<1);
      out[count] = &output;

      for (int i=1;i<inputLength;i++)
      {
	 output[inputLength+i] = output[inputLength-i] = in[i];
      }
      output[0] = 0;
      output[inputLength] = in[0];
      
   }

      
};
