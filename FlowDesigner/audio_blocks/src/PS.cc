// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

using namespace std;

namespace FD {

class PS;

DECLARE_NODE(PS)
/*Node
 *
 * @name PS
 * @category DSP:TimeFreq
 * @description Converts the output of the FFT (not RFFT) node to a power spectrum
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Spectrum output from FFT
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Power spectrum (half the input length)
 *
END*/


class PS : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   PS(string nodeName, ParameterSet params)
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
      int outputLength = inputLength >> 1;

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;
      
      output[0]=in[0]*in[0];
      for (int i=1;i<outputLength;i++)
      {
         output[i]=in[i]*in[i]+in[inputLength-i]*in[inputLength-i];
      }
   }

};

}//namespace FD
