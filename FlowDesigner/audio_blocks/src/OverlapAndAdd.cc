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

class OverlapAndAdd;

//DECLARE_NODE(OverlapAndAdd)
NODE_INFO(OverlapAndAdd, "Signal:Audio", "INPUT", "OUTPUT", "INPUTLENGTH:OUTPUTLENGTH")

class OverlapAndAdd : public FrameOperation {
   
   int inputID;
   int inputLength;

public:
   OverlapAndAdd(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      inputID = addInput("INPUT");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));
      inputsCache[inputID].lookBack = 1;
   }

   ~OverlapAndAdd() {}

   virtual void specificInitialize()
   {
      this->FrameOperation::specificInitialize();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      if (count <= 0)
      {
         out[count]->status = Object::before_beginning;
         return;
      }
      NodeInput input = inputs[inputID];
      ObjectRef inputValue = input.node->getOutput(input.outputID, count);
      ObjectRef pastInputValue = input.node->getOutput(input.outputID, count-1);

      Vector<float> &output = object_cast<Vector<float> > (out[count]);
      if (inputValue->status != Object::valid)
      {
         output.status = inputValue->status;
         return;
      }
      if (pastInputValue->status != Object::valid)
      {
         output.status = pastInputValue->status;
         return;
      }

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      const Vector<float> &past = object_cast<Vector<float> > (pastInputValue);
      
      for (int i=0;i<outputLength;i++)
      {
         output[i]=in[i]+past[i+outputLength];
      }
      
      output.status = Object::valid;
   }

};
