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

class Dist;

//DECLARE_NODE(Dist)
NODE_INFO(Dist, "Signal:DSP", "INPUT:REF", "OUTPUT", "INPUTLENGTH:OUTPUTLENGTH")

class Dist : public FrameOperation {
   
   int refID;
   int inputID;
   int inputLength;

   static float cos_dist(const float *x1, const float *x2, int len)
   {
      double xx=0,xy=0,yy=0;
      double sx=0,sy=0;
      for (int i=0;i<len;i++)
      {
	 sy+=x2[i];
	 sx+=x1[i];
      }
      sx/=len;
      sy/=len;
      for (int i=0;i<len;i++)
      {
	 //cerr << x1[i] << " " << x2[i] << endl;
	 xx+=(x1[i]-sx)*(x1[i]-sx);
	 xy+=(x1[i]-sx)*(x2[i]-sy);
	 yy+=(x2[i]-sy)*(x2[i]-sy);
      }
      return xy/sqrt(xx*yy);
   }
/*  
   static float cos_dist(const float *x1, const float *x2, int len)
   {
      double xx=0,xy=0,yy=0;
      double sx=0,sy=0;
      for (int i=0;i<len;i++)
      {
	 sy+=x2[i];
	 sx+=x1[i];
      }
      sx/=len;
      sy/=len;
      for (int i=0;i<len;i++)
      {
	 //cerr << x1[i] << " " << x2[i] << endl;
	 xx+=(x1[i])*(x1[i]);
	 xy+=(x1[i])*(x2[i]);
	 yy+=(x2[i])*(x2[i]);
      }
      return xy/sqrt(xx*yy);
   }
*/
public:
   Dist(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      inputID = addInput("INPUT");
      refID = addInput("REF");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));
   }

   ~Dist() {}

   virtual void specificInitialize()
   {
      this->FrameOperation::specificInitialize();
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
      
      NodeInput refInput = inputs[refID];
      ObjectRef refValue = refInput.node->getOutput(refInput.outputID, count);
     
      Vector<ObjectRef> &ref = object_cast<Vector<ObjectRef> > (refValue);
      for (int i=0;i<outputLength;i++)
      {
	 output[i]=cos_dist(in.begin(), object_cast<Vector<float> > (ref[i]).begin(), in.size());
	 }
      
      /*NodeInput refInput = inputs[refID];
      ObjectRef refValue = refInput.node->getOutput(refInput.outputID, count);
     
      Vector<float> &ref = object_cast<Vector<float> > (refValue);
      for (int i=0;i<outputLength;i++)
      {
	 output[i]=cos_dist(in.begin(), ref.begin(), inputLength);
	 }*/

      output.status = Object::valid;
   }

};
