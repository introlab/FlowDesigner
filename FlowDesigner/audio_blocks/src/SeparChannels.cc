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
#include "RotatingFrameBuffer.h"
#include "GrowingFrameBuffer.h"

class SeparChannels;

//DECLARE_NODE(SeparChannels)
NODE_INFO(SeparChannels, "Signal:Audio", "NUM:DEN", "OUTPUT", "INPUTLENGTH:OUTPUTLENGTH")

class SeparChannels : public BufferedNode {
   
   int inputID;
   int output1ID;
   int output2ID;
   int inputLength;
   int outputLength;

public:
   SeparChannels(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      output1ID = addInput("LEFT");
      output2ID = addInput("RIGHT");
      inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      outputLength = dereference_cast<int> (parameters.get("OUTPUTLENGTH"));
   }

   ~SeparChannels() {}

   void SeparChannels::initializeBuffers()
   {
      for (int i=0;i<outputs.size();i++)
      {
	 if (outputs[i].cacheAll)
	 {
	    outputs[i].buffer = ObjectRef(new GrowingFrameBuffer<float> (outputLength, outputs[i].lookAhead+outputs[i].lookBack+1));
	 } else {
	    outputs[i].buffer = ObjectRef(new RotatingFrameBuffer<float> (outputLength, outputs[i].lookAhead+outputs[i].lookBack+1));
	 }
      }
      //output=outputs[outputID].buffer;
      
   }

   virtual void specificInitialize()
   {
      this->BufferedNode::specificInitialize();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      NodeInput input = inputs[inputID];
      ObjectRef inputValue = input.node->getOutput(input.outputID, count);

      Vector<float> &output = object_cast<Vector<float> > (out[count]);
      if (inputValue->status != Object::valid)
      {
         output.status = inputValue->status;
         return;
      }

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      
      int channel;
      if (output_id == output1ID)
	 channel = 0;
      else 
	 channel = 1;
      for (int i=0;i<outputLength;i++)
      {
	 output[i] = in[2*i+channel];
      }
      
      output.status = Object::valid;
   }

};
