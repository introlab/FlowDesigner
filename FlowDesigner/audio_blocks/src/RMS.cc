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

class RMS;

DECLARE_NODE(RMS)
/*Node
 *
 * @name RMS
 * @category Signal:DSP
 * @description Root mean squared (RMS) value of a signal
 *
 * @input_name INPUT
 * @input_description The input signal
 *
 * @output_name OUTPUT
 * @output_description The RMS value
 *
END*/


class RMS : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   RMS(string nodeName, ParameterSet params)
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

      Vector<float> &output = *Vector<float>::alloc(1);
      out[count] = &output;

      double energy=0;
      for (int i=0;i<inputLength;i++)
      {
	 energy+=in[i]*in[i];
      }

      output[0]=sqrt(energy/inputLength);
   }

};
