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

#include <stream.h>
#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

class Index2Vector;

DECLARE_NODE(Index2Vector)
/*Node
 *
 * @name Index2Vector
 * @category Signal:Base
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name LENGTH
 * @parameter_description No description available
 *
END*/


class Index2Vector : public BufferedNode {
   
   int inputID;
   int outputID;
   int length;

public:
   Index2Vector(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      length = dereference_cast<int> (parameters.get("LENGTH"));
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
      int index = int(in[0]);
      if (index >= length || index < 0)
	 throw new NodeException(this, "Index out of range", __FILE__, __LINE__);
      //int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(length);
      out[count] = &output;

      
      for (int i=0;i<length;i++)
      {
         output[i]=0;
      }
      output[index] = 1;
   }

};
