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
#include "FrameOperation.h"
#include "Buffer.h"
#include "Vector.h"

class MergeChannels;

//DECLARE_NODE(MergeChannels)
NODE_INFO(MergeChannels, "Signal:Audio", "NUM:DEN", "OUTPUT", "INPUTLENGTH:OUTPUTLENGTH")

class MergeChannels : public FrameOperation {
   
   int input1ID;
   int input2ID;
   int inputLength;

public:
   MergeChannels(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      input1ID = addInput("LEFT");
      input2ID = addInput("RIGHT");
      inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
   }

   ~MergeChannels() {}

   virtual void specificInitialize()
   {
      this->FrameOperation::specificInitialize();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      NodeInput input1 = inputs[input1ID];
      ObjectRef input1Value = input1.node->getOutput(input1.outputID, count);

      Vector<float> &output = object_cast<Vector<float> > (out[count]);
      if (input1Value->status != Object::valid)
      {
         output.status = input1Value->status;
         return;
      }

      NodeInput input2 = inputs[input2ID];
      ObjectRef input2Value = input2.node->getOutput(input2.outputID, count);
      if (input2Value->status != Object::valid)
      {
         output.status = input2Value->status;
         return;
      }

      const Vector<float> &in1 = object_cast<Vector<float> > (input1Value);
      const Vector<float> &in2 = object_cast<Vector<float> > (input2Value);
      
      for (int i=0;i<inputLength;i++)
      {
	 output[2*i] = in1[i];
	 output[2*i+1] = in2[i];
      }
      
      output.status = Object::valid;
   }

};
