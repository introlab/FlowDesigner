// Copyright (C) 2001 Locus Dialog (author: Jean-Marc Valin)

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

class FrameLabel;

DECLARE_NODE(FrameLabel)
/*Node
 *
 * @name FrameLabel
 * @category DSP:Audio
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
      

      int sample = (count*frameAdv)+1;
      while (sample > currEnd)
      {
	 IStream &file = object_cast<IStream> (inputValue);
	 currentTag = new String();
	 file >> currStart;
	 if (currStart != currEnd+1)
	    throw new NodeException (this, "Start and end label don't fit",__FILE__, __LINE__);
	 file >> currEnd;
	 //This had to be changed, not sure why
	 //file >> *currentTag;
	 string tmp;
	 file >> tmp;
	 *currentTag = tmp;
      }

      out[count] = currentTag;
   }

      
};
