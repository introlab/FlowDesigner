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

class BWExpan;

DECLARE_NODE(BWExpan)
/*Node
 *
 * @name BWExpan
 * @category Signal:DSP
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name GAMMA
 * @parameter_description No description available
 *
END*/


class BWExpan : public BufferedNode {
   
   int inputID;
   int outputID;
   vector<float> gains;
   float gamma;

public:
   BWExpan(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      , gains(1,1)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      gamma = dereference_cast<float> (parameters.get("GAMMA"));
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

      
      for (int i=gains.size();i<inputLength;i++)
	 gains.push_back(gamma*gains[i-1]);
      
      for (int i=0;i<inputLength;i++)
      {
         output[i]=gains[i]*in[i];
      }
      
   }

      
};
