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


class FFT;

DECLARE_NODE(FFT)
/*Node
 *
 * @name FFT
 * @category Signal:DSP
 * @description Computes the real FFT of a float vector
 *
 * @input_name INPUT
 * @input_type Vector
 * @input_description The input vector
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description The FFT resuls as [r(0), r(1),..., r(N/2), i(N/2-1), ..., i(2), i(1)]
 *
END*/


class FFT : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   FFT(string nodeName, ParameterSet params)
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
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;
      
      FFTWrap.rfft(&in[0], &output[0], inputLength);
   }

};
