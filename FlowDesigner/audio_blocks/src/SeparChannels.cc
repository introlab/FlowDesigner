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
#include "Vector.h"

class SeparChannels;

DECLARE_NODE(SeparChannels)
/*Node
 *
 * @name SeparChannels
 * @category Signal:Audio
 * @description No description available
 *
 * @input_name INPUT
 * @input_type Vector
 * @input_description Stero frame (encoded as left, right, left, right, ...)
 *
 * @output_name LEFT
 * @output_type Vector
 * @output_description Frame for the left channel
 *
 * @output_name RIGHT
 * @output_type Vector
 * @output_description Frame for the right channel
 *
END*/


class SeparChannels : public BufferedNode {
   
   int inputID;
   int output1ID;
   int output2ID;

public:
   SeparChannels(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      output1ID = addOutput("LEFT");
      output2ID = addOutput("RIGHT");
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
      int outputLength = inputLength>>1;

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;
      
      int channel;
      if (output_id == output1ID)
	 channel = 0;
      else 
	 channel = 1;
      for (int i=0;i<outputLength;i++)
      {
	 output[i] = in[2*i+channel];
      }
   }

};
