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

class HistoVect;

//DECLARE_NODE(HistoVect)
NODE_INFO(HistoVect, "Signal:Misc", "INPUT", "OUTPUT", "LENGTH")

class HistoVect : public FrameOperation {
   
   int inputID;

public:
   HistoVect(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      inputID = addInput("INPUT");
   }

   ~HistoVect() {}

   virtual void specificInitialize()
   {
      this->FrameOperation::specificInitialize();
   }

      //virtual void request(int outputID, const ParameterSet &req) {inputs[inputID].node->request(outputID,req);}
      
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
      int &in = dereference_cast<int> (inputValue);
      
      for (int i=0;i<outputLength;i++)
         output[i]=0;
      output[in]=1;
      
      output.status = Object::valid;
   }

};
