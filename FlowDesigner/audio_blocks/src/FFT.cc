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

#include "fftw.h"
#include "rfftw.h"
#include <stream.h>
#include "FrameOperation.h"
#include "Buffer.h"
#include "Vector.h"

class FFT;

//DECLARE_NODE(FFT)
NODE_INFO(FFT,"Signal", "INPUT", "OUTPUT", "LENGTH")

class FFT : public FrameOperation {
   
   int inputID;
   rfftw_plan plan;
   int inputLength;

public:
   FFT(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      inputID = addInput("INPUT");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));
   }

   ~FFT() {rfftw_destroy_plan(plan);}

   virtual void specificInitialize()
   {
      plan = rfftw_create_plan (inputLength, FFTW_FORWARD, FFTW_ESTIMATE);
      this->FrameOperation::specificInitialize();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      NodeInput input = inputs[inputID];
      ObjectRef inputValue = input.node->getOutput(input.outputID, count);
      //Ptr<Vector<float> > inputValue = input.node->getOutput(input.outputID, count);

      Vector<float> &output = object_cast<Vector<float> > (out[count]);
      if (inputValue->status != Object::valid)
      {
         output.status = inputValue->status;
         return;
      }
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      rfftw_one (plan, const_cast <float *> (in.begin()), output.begin());
      //rfftw_one (plan, const_cast <float *> (inputValue->begin()), output.begin());
      output.status = Object::valid;
   }

};
