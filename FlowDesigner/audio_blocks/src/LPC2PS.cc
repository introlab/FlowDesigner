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
#include <stdlib.h>
#include <fftw.h>
#include <rfftw.h>
#include <math.h>

class LPC2PS;

//DECLARE_NODE(LPC2PS)
NODE_INFO(LPC2PS,"Signal:DSP", "INPUT", "OUTPUT", "INPUTLENGTH:OUTPUTLENGTH")

class LPC2PS : public FrameOperation {
   
   int inputID;
   int inputLength;
   rfftw_plan plan1;
   rfftw_plan plan2;
   float *hamming;
   int SAMP_SIZE;
   int SAMP_SIZE_2;

public:
   LPC2PS(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      inputID = addInput("INPUT");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));

      SAMP_SIZE_2 = outputLength;
      SAMP_SIZE   = 2 * SAMP_SIZE_2;

   }

   ~LPC2PS() {rfftw_destroy_plan(plan1); rfftw_destroy_plan(plan2); delete hamming;}

   virtual void specificInitialize()
   {
      plan1 = rfftw_create_plan (SAMP_SIZE, FFTW_FORWARD, FFTW_ESTIMATE);
      plan2 = rfftw_create_plan (SAMP_SIZE, FFTW_BACKWARD, FFTW_ESTIMATE);
      this->FrameOperation::specificInitialize();
      hamming = new float[SAMP_SIZE];
      for (int i=0;i<SAMP_SIZE;i++)
         hamming[i]= 0.54 - 0.46*cos(2*M_PI*i/float(SAMP_SIZE));
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
      
      float response[SAMP_SIZE];
      float ps[SAMP_SIZE];

      for (int i=0;i<min(int(in.size()),SAMP_SIZE);i++)
         response[i]=in[i];
      for (int i=in.size();i<SAMP_SIZE;i++)
         response[i]=0;

      rfftw_one(plan1, response, ps);
      
      ps[0]=ps[0]*ps[0];
      for (int i=1;i<SAMP_SIZE_2;i++)
         ps[i]=ps[i]*ps[i]+ps[SAMP_SIZE-i]*ps[SAMP_SIZE-i];
      for (int i=SAMP_SIZE_2;i<SAMP_SIZE;i++)
         ps[i]=0.0;
      for (int i=0;i<SAMP_SIZE_2;i++)
         output[i]=1/ps[i];


      output.status = Object::valid;

   }

};
