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
#include "lpc.h"
#include <stdlib.h>
#include <math.h>

class LPC;

//DECLARE_NODE(LPC)
NODE_INFO(LPC,"Signal:DSP", "INPUT", "OUTPUT", "INPUTLENGTH:OUTPUTLENGTH")

class LPC : public FrameOperation {
   
   int inputID;
   int inputLength;
   float *r;
   float *rc;
public:
   LPC(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      inputID = addInput("INPUT");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));
      r=new float[outputLength];
      rc=new float[outputLength];
   }

   ~LPC() {delete [] r; delete [] rc;}

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
            
      //vector<float> r(outputLength+1,0.0);
      //vector<float> filter(outputLength+1,0.0);
      autocorr(in.begin(), r, outputLength-1, in.size());
      float er=0;
      r[0] *= 1.001;
      r[0] += 1; //just in case of a null frame
      wld(output.begin(), r, rc, outputLength-1);
      for (int i=0;i<outputLength;i++)
        output[i] *= pow(.99,i);
      output.status = Object::valid;
   }

};
