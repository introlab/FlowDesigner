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

class FFTFlip;

DECLARE_NODE(FFTFlip)
/*Node
 *
 * @name FFTFlip
 * @category Signal:Base
 * @description Flips a half-spectrum to produce a symetric spectrum
 *
 * @input_name INPUT
 * @input_type Vector
 * @input_description Half spectrum (real)
 *
 * @output_name OUTPUT
 * @output_type Vector
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

      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
         return;
      }
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
