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

#include "Node.h"
#include "GrowingFrameBuffer.h"

class Accumulate;

DECLARE_NODE(Accumulate)
/*Node
 *
 * @name Accumulate
 * @category General
 * @description Accumulation of frames into a buffer
 *
 * @input_name INPUT
 * @input_description Input object
 * @input_type Vector
 *
 * @input_name ACCUM
 * @input_description Accumulator where to put the input
 * @input_type Vector
 *
 * @output_name OUTPUT
 * @output_description The input accumulator
 * @output_type Buffer
 *
END*/

class Accumulate : public Node {
protected:
   int inputID;
   int accumID;
   int outputID;
   //int processCount;
public:
   Accumulate (string nodeName, const ParameterSet &params)
      : Node(nodeName, params)
   {
      inputID = addInput("INPUT");
      accumID = addInput("ACCUM");
      outputID = addOutput("OUTPUT");
   }

   void specificInitialize()
   {
      //processCount = -1;
      Node::specificInitialize();
   }

   void reset()
   {
      //processCount = -1;
      Node::reset();
   }

   ObjectRef getOutput(int output_id, int count)
   {
      int i,j;
      //for (i=processCount+1;i<=count;i++)
      //{
	 ObjectRef inputValue = getInput(inputID,i);
	 if (inputValue->status == Object::valid)
	 {
	    ObjectRef accumValue = getInput(accumID,i);
	    Vector<ObjectRef> &accum = object_cast<Vector<ObjectRef> > (accumValue);
	    
	    accum.push_back(inputValue);
	 }
	 
      //}
      //processCount = count;
      return getInput(accumID,count);
   }


};
