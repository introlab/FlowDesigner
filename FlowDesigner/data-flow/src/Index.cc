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

class Index;

DECLARE_NODE(Index)
/*Node
 *
 * @name Index
 * @category Vector
 * @description Returns a float value from a vector
 *
 * @input_name INPUT
 * @input_type Vector
 * @input_description The input vector 
 *
 * @input_name INDEX
 * @input_type int
 * @input_description Index value (if not specified in parameter)
 *
 * @output_name OUTPUT
 * @output_type float
 * @output_description Float at a certain index
 *
 * @parameter_name INDEX
 * @parameter_type int
 * @parameter_description Index value
 *
END*/


class Index : public BufferedNode {
   
   int inputID;
   int outputID;
   int indexID;
   int index;

public:
   Index(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");

      if (parameters.exist("INDEX"))
      {
	 index = dereference_cast<int> (parameters.get("INDEX"));
      } else {
	 index=-1;
	 indexID = addInput("INDEX");
      }
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

      int ind;

      if (index == -1)
      {
	 ObjectRef indexValue = getInput(indexID, count);
	 if (indexValue->status != Object::valid)
	 {
	    out[count] = indexValue;
	    return;
	 }
	 ind = dereference_cast<int> (indexValue);
      } else {
	 ind = index;
      }

      if (ind >= inputLength)
	 throw new NodeException(this, "Index larger than vector size", __FILE__, __LINE__);

      out[count] = Float::alloc(in[ind]);
   }

      
};
