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
#include "vec.h"

class PackFrames;

DECLARE_NODE(PackFrames)
/*Node
 *
 * @name PackFrames
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
 * @parameter_name BACK
 * @parameter_description No description available
 *
 * @parameter_name FRONT
 * @parameter_description No description available
 *
END*/


class PackFrames : public BufferedNode {
   
   int inputID;
   int outputID;
   int front;
   int back;
   int length;

public:
   PackFrames(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      length = dereference_cast<int> (parameters.get("LENGTH"));
      front = dereference_cast<int> (parameters.get("FRONT"));
      back = dereference_cast<int> (parameters.get("BACK"));

      inputsCache[inputID].lookAhead=front;
      inputsCache[inputID].lookBack=back;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      int cnt=0;
      ObjectRef inputValue;
      Vector<float> &output = *Vector<float>::alloc(length*(front+back+1));
      out[count] = &output;
      for (int i=-back;i<=front;i++)
      {
	 
	 if (count+i >= 0)
	    inputValue = getInput(inputID, count+i);
	 else
	    inputValue = Object::nilObject;
	 if (inputValue->status != Object::valid)
	 {
	    for (int j=0;j<length;j++)
	       output[cnt++] = 0.0;
	 } else {
	    const Vector<float> &in = object_cast<Vector<float> > (inputValue);
	    for (int j=0;j<length;j++)
	       output[cnt++] = in[j];
	 
	 }
      }      
   }

      
};
