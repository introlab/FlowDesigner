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

class Length;

DECLARE_NODE(Length)
/*Node
 *
 * @name Length
 * @category Vector
 * @description Get the length of a vector
 *
 * @input_name INPUT
 * @input_description The vector input
 * @input_type Vector
 *
 * @output_name OUTPUT
 * @output_description The length of the vector
 * @output_type int
 *
END*/


class Length : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   Length(string nodeName, ParameterSet params)
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

      int sz;
      if (typeid(*inputValue) == typeid(Vector<ObjectRef>))
      {
	 //cerr << "Vector<ObjectRef>\n";
	 sz = object_cast<Vector<ObjectRef> > (inputValue).size();
      } else if (typeid(*inputValue) == typeid(Vector<float>))
      {
	 //cerr << "Vector<float>\n";
	 sz = object_cast<Vector<float> > (inputValue).size();
      } else {
	 //cerr << "error!!!\n";
	 throw new NodeException(this, "Unknown input type", __FILE__, __LINE__);
      }

      out[count] = new Int (sz);

   }

      
};
