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
#include <fftw.h>
#include <rfftw.h>

class DCT;

DECLARE_NODE(DCT)

class DCT : public FrameOperation {
   
   int inputID;
   int inputLength;

   rfftw_plan plan;
   float *inputCopy;
   float *outputCopy;
   float normalize;

public:
   DCT(string nodeName, ParameterSet params)
      : FrameOperation(nodeName, params)

   {
      inputID = addInput("INPUT");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));
   
      inputCopy = new float [inputLength*2];
      outputCopy =new float [inputLength*2];
      normalize = .5/inputLength;
      plan = rfftw_create_plan (inputLength*2, FFTW_FORWARD, FFTW_ESTIMATE);
}

   ~DCT() {
      rfftw_destroy_plan(plan);
      delete [] inputCopy;
      delete [] outputCopy;
   }

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
      
      int i,j;
      for (i=0, j=inputLength*2-1 ;i<inputLength ; i++, j--)
      {
         inputCopy[i]=inputCopy[j] = in[i];
      }
      rfftw_one(plan, inputCopy, outputCopy);
      for (i=0;i<outputLength;i++)
      {
         output[i]=normalize*outputCopy[i];
      }
      
      output.status = Object::valid;
   }

};
