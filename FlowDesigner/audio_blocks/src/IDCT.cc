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
#include <math.h>
#include "FFTWrap.h"

class IDCT;

DECLARE_NODE(IDCT)
/*Node

 * @name IDCT
 * @category Signal:DSP
 * @description No description available

 * @input_name INPUT
 * @input_description No description available

 * @output_name OUTPUT
 * @output_description No description available

 * @parameter_name LENGTH
 * @parameter_description No description available

END*/


class IDCT : public BufferedNode {
   
   int inputID;
   int outputID;
   int length;

   fft_complex *inputCopy;
   fft_complex *outputCopy;
   float *rNormalize;
   float *iNormalize;

public:
   IDCT(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)

   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      length = dereference_cast<int> (parameters.get("LENGTH"));

      if (length & 1) 
      {
	 throw NodeException(NULL, "IDCT only implemented for even sizes", __FILE__, __LINE__);
      }

      inputCopy = new fft_complex [length];
      outputCopy =new fft_complex [length];
      rNormalize =new float [length];
      iNormalize =new float [length];
      float sqrt2n=sqrt(2.0/length);
      for (int i=0;i<length;i++)
      {
	 rNormalize[i]=cos(M_PI*i/(2*length))*sqrt2n;
	 iNormalize[i]=sin(M_PI*i/(2*length))*sqrt2n;
      }
      rNormalize[0] /= sqrt(2);

}

   ~IDCT() 
   {
      delete [] inputCopy;
      delete [] outputCopy;
      delete [] rNormalize;
      delete [] iNormalize;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
         return;
      }
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);

      Vector<float> &output = *Vector<float>::alloc(length);
      out[count] = &output;
            
      int i,j;


      for (i=0;i<length;i++)
      {
	 inputCopy[i].re = rNormalize[i]*in[i];
	 inputCopy[i].im = iNormalize[i]*in[i];
      }

      FFTWrap.ifft(inputCopy, outputCopy, length);

      for (i=0, j=0 ;i<length ; i+=2, j++)
         output[i]=outputCopy[j].re;

      for (i = length-1; i>=0 ; i-=2, j++)
         output[i]=outputCopy[j].re;
   }

};
