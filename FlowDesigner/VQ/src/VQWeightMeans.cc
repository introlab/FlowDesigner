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
#include "kmeans.h"

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

class VQWeightMeans;

//DECLARE_NODE(VQWeightMeans)
NODE_INFO(VQWeightMeans,"VQ", "INPUT:VQ", "OUTPUT", "INPUTLENGTH:OUTPUTLENGTH")

class VQWeightMeans : public FrameOperation {
   
   int inputID;
   int VQinputID;
   int inputLength;

public:
   VQWeightMeans(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      inputID = addInput("INPUT");
      VQinputID = addInput("VQ");
      /*if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
         else inputLength = dereference_cast<int> (parameters.get("LENGTH"));*/
   }

   ~VQWeightMeans() {}

   virtual void specificInitialize()
   {
      this->FrameOperation::specificInitialize();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      NodeInput input = inputs[inputID];
      NodeInput VQInput = inputs[VQinputID];

      ObjectRef VQValue = VQInput.node->getOutput(VQInput.outputID, count);

      Vector<float> &output = object_cast<Vector<float> > (out[count]);
      if (VQValue->status != Object::valid)
      {
         output.status = VQValue->status;
         return;
      }

      ObjectRef inputValue = input.node->getOutput(input.outputID, count);

      if (inputValue->status != Object::valid)
      {
         output.status = inputValue->status;
         return;
      }
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      const KMeans &vq = object_cast<KMeans> (VQValue);
      
      //int classID = vq.getClassID(in.begin());
      //const vector<float> &mean = vq[classID];
      vector<float> scal(inputLength);
      for (int i=0;i<inputLength;i++)
      {
	 float tmp = .5*(in[i]+1);
	 scal[i] = tmp*tmp;
      }

      vq.weightMeans(in, output);

      output.status = Object::valid;
   }

};
