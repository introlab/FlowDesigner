// Copyright (C) 1999 Jean-Marc Valin
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <map>
#include "FFTWrap.h"
#include <complex>

class IRFFT;

DECLARE_NODE(IRFFT)
/*Node
 *
 * @name IRFFT
 * @category Signal:DSP
 * @description Inverse FFT, half complex to real vector
 *
 * @input_name INPUT
 * @input_type Vector
 * @input_description Half complex vector
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description Real inverse FFT output
 *
END*/


class IRFFT : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   IRFFT(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
         return;
      }
      const Vector<complex<float> > &in = object_cast<Vector<complex<float> > > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(inputLength<<1);
      out[count] = &output;
      
      float fft_in[inputLength<<1];

      fft_in[0] = in[0].real();
      fft_in[inputLength] = 0;
      for (int i=1;i<inputLength;i++)
      {
	 fft_in[i] = in[i].real();
	 fft_in[(inputLength<<1)-i] = in[i].imag();
      }

      FFTWrap.irfft(fft_in, &output[0], inputLength<<1);
      
   }

};
