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

class Select;

DECLARE_NODE(Select)
/*Node
 *
 * @name Select
 * @category Signal:Manip
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name START
 * @parameter_description No description available
 *
 * @parameter_name END
 * @parameter_description No description available
 *
END*/


class Select : public BufferedNode {
   
   int inputID;
   int outputID;

   int start;
   int end;

public:
   Select(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)

   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      
      start = dereference_cast<int>(parameters.get("START"));
      end = dereference_cast<int> (parameters.get("END"));
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
      int outputLength = end-start+1;

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;

      //cerr << "Select " << name << " " << &output << " " << count << " " << inputLength << " " << end << " " << outputLength << endl;
      if (inputLength <= end)
	 throw new NodeException(this, "Input vector too short", __FILE__, __LINE__);

      int i,j;
      for (i=start, j = 0 ;i<=end;i++,j++)
      {
         output[j]=in[i];
      }
   }

};
