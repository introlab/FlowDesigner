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
#include "FFNet.h"

class NNetExec;

//DECLARE_NODE(NNetExec)
NODE_INFO(NNetExec,"NNet", "INPUT:NNET", "OUTPUT", "INPUTLENGTH:OUTPUTLENGTH")

class NNetExec : public FrameOperation {
   
   int inputID;
   int netInputID;
   int inputLength;

public:
   NNetExec(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      inputID = addInput("INPUT");
      netInputID = addInput("NNET");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH")); 
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));
   }

   ~NNetExec() {}

   virtual void specificInitialize()
   {
      this->FrameOperation::specificInitialize();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      NodeInput input = inputs[inputID];
      NodeInput netInput = inputs[netInputID];

      ObjectRef netValue = netInput.node->getOutput(netInput.outputID, count);

      Vector<float> &output = object_cast<Vector<float> > (out[count]);
      if (netValue->status != Object::valid)
      {
         output.status = netValue->status;
         return;
      }

      ObjectRef inputValue = input.node->getOutput(input.outputID, count);

      if (inputValue->status != Object::valid)
      {
         output.status = inputValue->status;
         return;
      }
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      FFNet &net = object_cast<FFNet> (netValue);
      
      //int classID = vq.getClassID(in.begin());
      //const vector<float> &mean = vq[classID];
      double tmp[in.size()];
      for (int i=0;i<in.size();i++)
	 tmp[i]=in[i];
      double *netOut = net.calc(tmp);
      
      for (int i=0;i<outputLength;i++)
         output[i]=netOut[i];
       
      output.status = Object::valid;
   }

};
