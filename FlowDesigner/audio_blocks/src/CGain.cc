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

class CGain;

//DECLARE_NODE(CGain)
NODE_INFO(CGain,"Signal:Base", "INPUT:GAIN", "OUTPUT", "LENGTH")

class CGain : public FrameOperation {
   
   int inputID;
   int gainID;

   int inputLength;
      //float gain;

public:
   CGain(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      inputID = addInput("INPUT");
      gainID = addInput("GAIN");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));
      //   gain = dereference_cast<float> (parameters.get("GAIN"));
   }

   ~CGain() {}

   virtual void specificInitialize()
   {
      this->FrameOperation::specificInitialize();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      NodeInput input = inputs[inputID];
      ObjectRef inputValue = input.node->getOutput(input.outputID, count);

      NodeInput gainInput = inputs[gainID];
      ObjectRef gainValue = gainInput.node->getOutput(gainInput.outputID, count);

      Vector<float> &output = object_cast<Vector<float> > (out[count]);
      if (inputValue->status != Object::valid)
      {
         output.status = inputValue->status;
         return;
      }
      if (gainValue->status != Object::valid)
      {
         output.status = gainValue->status;
         return;
      }

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      const Vector<float> &gain = object_cast<Vector<float> > (gainValue);

      float g=gain[0];

      if (g>10) g=10;
      if (g<-10) g=-10;

      for (int i=0;i<outputLength;i++)
      {
         output[i]=gain[0]*in[i];
      }
      
      output.status = Object::valid;
   }

};
