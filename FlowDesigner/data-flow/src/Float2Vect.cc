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

class Float2Vect;

DECLARE_NODE(Float2Vect)
/*Node
 *
 * @name Float2Vect
 * @category Vector
 * @description Converts a float value to a vector of 1 element
 *
 * @input_name INPUT
 * @input_description The input float 
 * @input_type float
 *
 * @output_name OUTPUT
 * @output_description The vector
 * @output_type Vector
 *
END*/


class Float2Vect : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   Float2Vect(string nodeName, ParameterSet params)
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
      const float &in = dereference_cast<float> (inputValue);

      Vector<float> &output = *Vector<float>::alloc(1);
      out[count] = &output;
      output[0] = in;

      
   }

      
};
