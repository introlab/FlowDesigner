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
#include "NNetSet.h"

class NNetSetCalc;

DECLARE_NODE(NNetSetCalc)
/*Node

 * @name NNetSetCalc
 * @category NNet
 * @description No description available

 * @input_name INPUT
 * @input_description No description available

 * @input_name ID
 * @input_description No description available

 * @input_name NNET
 * @input_description No description available

 * @output_name OUTPUT
 * @output_description No description available

 * @parameter_name INPUTLENGTH
 * @parameter_description No description available

 * @parameter_name OUTPUTLENGTH
 * @parameter_description No description available

END*/


class NNetSetCalc : public FrameOperation {
   
   int inputID;
   int netInputID;
   int IDInputID;
   int inputLength;

public:
   NNetSetCalc(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      inputID = addInput("INPUT");
      netInputID = addInput("NNET");
      IDInputID = addInput("ID");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH")); 
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));
   }

   ~NNetSetCalc() {}

   virtual void specificInitialize()
   {
      this->FrameOperation::specificInitialize();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      NodeInput input = inputs[inputID];
      NodeInput netInput = inputs[netInputID];
      NodeInput idInput = inputs[IDInputID];

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

      ObjectRef idValue = idInput.node->getOutput(idInput.outputID, count);

      if (idValue->status != Object::valid)
      {
         output.status = idValue->status;
         return;
      }

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      NNetSet &net = object_cast<NNetSet> (netValue);
      const Vector<float> &id = object_cast<Vector<float> > (idValue);
      
      //int classID = vq.getClassID(in.begin());
      //const vector<float> &mean = vq[classID];
      double tmp[in.size()];
      for (int i=0;i<in.size();i++)
	 tmp[i]=in[i];
      double *netOut = net.calc(int(floor(id[0])), tmp);
      
      for (int i=0;i<outputLength;i++)
         output[i]=netOut[i];
       
      output.status = Object::valid;
   }

};
