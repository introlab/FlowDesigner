// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <map>
#include "FFTWrap.h"
#include <complex>

class RFFT;

DECLARE_NODE(RFFT)
/*Node
 *
 * @name RFFT
 * @category DSP:TimeFreq
 * @require FFT
 * @description Computes the FFT of a real input vector and output a complex result
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Real vector
 *
 * @output_name OUTPUT
 * @output_type Vector<complex>
 * @output_description Complex FFT output
 *
END*/


class RFFT : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   RFFT(string nodeName, ParameterSet params)
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

      Vector<complex<float> > &output = *Vector<complex<float> >::alloc((inputLength+1)>>1);
      out[count] = &output;
      
      DYN_VEC(float, inputLength, fft_out);
      //float fft_out[inputLength];
      
      FFTWrap.rfft(&in[0], fft_out, inputLength);
      output[0] = fft_out[0];
      for (int i=1;i<(inputLength+1)>>1;i++)
      {
	 output[i] = complex<float>(fft_out[i], fft_out[inputLength-i]);
      }
      
   }

};
