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

class DCT;

DECLARE_NODE(DCT)
/*Node
 *
 * @name DCT
 * @category Signal:DSP
 * @description Computes the Discrete Cosine Transform (DCT)
 *
 * @input_name INPUT
 * @input_description The input vector
 *
 * @output_name OUTPUT
 * @output_description The result of the DCT
 *
 * @parameter_name LENGTH
 * @parameter_type int
 * @parameter_description Length of the DCT
 *
END*/


class DCT : public BufferedNode {
   
   int inputID;
   int outputID;
   int length;

   vector<float> inputCopy;
   vector<float> outputCopy;
   vector<float> rNormalize;
   vector<float> iNormalize;

public:
   DCT(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      length = dereference_cast<int> (parameters.get("LENGTH"));

      if (length & 1) 
      {
	 throw new NodeException(NULL, "DCT only implemented for even sizes", __FILE__, __LINE__);
      }

      inputCopy.resize(length);
      outputCopy.resize(length);
      rNormalize.resize(length);
      iNormalize.resize(length);
      float sqrt2n=sqrt(2.0/length);
      for (int i=0;i<length;i++)
      {
	 rNormalize[i]=cos(M_PI*i/(2*length))*sqrt2n;
	 iNormalize[i]=-sin(M_PI*i/(2*length))*sqrt2n;
      }
      rNormalize[0] /= sqrt(2);

   }

      /*~DCT() 
   {
      delete [] inputCopy;
      delete [] outputCopy;
      delete [] rNormalize;
      delete [] iNormalize;
      }*/

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
      for (i=0, j=0 ;i<length ; i+=2, j++)
         inputCopy[j]=in[i];

      for (i = length-1; i>=0 ; i-=2, j++)
         inputCopy[j]=in[i];
      

      FFTWrap.rfft(&inputCopy[0], &outputCopy[0], length);

      for (i=1;i<length/2;i++)
      {
	 output[i]=rNormalize[i]*outputCopy[i] - iNormalize[i]*outputCopy[length-i];
	 output[length-i]=rNormalize[length-i]*outputCopy[i] + iNormalize[length-i]*outputCopy[length-i];
      }

      output[0]=outputCopy[0]*rNormalize[0];
      output[length/2] = outputCopy[length/2]*rNormalize[length/2];
   }

};
