// Copyright (C) 2001 Locus Dialog (author: Jean-Marc Valin)
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

class FrameLabel;

DECLARE_NODE(FrameLabel)
/*Node
 *
 * @name FrameLabel
 * @category Signal:Audio
 * @description Applies a gain to a vector
 *
 * @input_name INPUT
 * @input_type Stream
 * @input_description Input stream
 *
 * @output_name OUTPUT
 * @output_type String
 * @output_description Frame label
 *
 * @parameter_name FRAME_ADVANCE
 * @parameter_type int
 * @parameter_description Frame advance to use
 *
END*/


class FrameLabel : public BufferedNode {
   
   int inputID;
   int outputID;
   RCPtr<String> currentTag;
   int currStart;
   int currEnd;
   int frameAdv;

public:
   FrameLabel(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      frameAdv = dereference_cast<int> (parameters.get("FRAME_ADVANCE"));
      currentTag = new String();
      currStart=-1;
      currEnd=-1;
      inOrder=true;
   }

   void reset()
   {
      currStart=-1;
      currEnd=-1;
      BufferedNode::reset();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      //String &output = *Vector<float>::alloc(outputLength);
      //out[count] = &output;
      
      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
	 return;
      }

      int sample = (count*frameAdv)+1;
      while (sample > currEnd)
      {
	 Stream &file = object_cast<Stream> (inputValue);
	 currentTag = new String();
	 file >> currStart;
	 if (currStart != currEnd+1)
	    throw new NodeException (this, "Start and end label don't fit",__FILE__, __LINE__);
	 file >> currEnd;
	 file >> *currentTag;
      }

      out[count] = currentTag;
   }

      
};
