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
#include <math.h>

class DCT;

//DECLARE_NODE(DCT)
NODE_INFO(DCT, "Signal:DSP", "INPUT", "OUTPUT", "LENGTH")

class DCT : public FrameOperation {
   
   int inputID;
   int inputLength;

   rfftw_plan plan;
   float *inputCopy;
   float *outputCopy;
   float *rNormalize;
   float *iNormalize;

public:
   DCT(string nodeName, ParameterSet params)
      : FrameOperation(nodeName, params)

   {
      inputID = addInput("INPUT");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));

      if (inputLength & 1) 
      {
	 throw NodeException(NULL, "DCT only implemented for even sizes", __FILE__, __LINE__);
      }

      inputCopy = new float [inputLength];
      outputCopy =new float [inputLength];
      rNormalize =new float [inputLength];
      iNormalize =new float [inputLength];
      float sqrt2n=sqrt(2.0/inputLength);
      for (int i=0;i<inputLength;i++)
      {
	 rNormalize[i]=cos(M_PI*i/(2*inputLength))*sqrt2n;
	 iNormalize[i]=-sin(M_PI*i/(2*inputLength))*sqrt2n;
      }
      rNormalize[0] /= sqrt(2);

      //normalize = .5/inputLength;
      plan = rfftw_create_plan (inputLength, FFTW_FORWARD, FFTW_ESTIMATE);
}

   ~DCT() 
   {
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
      for (i=0, j=0 ;i<inputLength ; i+=2, j++)
         inputCopy[j]=in[i];

      for (i = inputLength-1; i>=0 ; i-=2, j++)
         inputCopy[j]=in[i];
      

      rfftw_one(plan, inputCopy, outputCopy);
      
      
      for (i=1;i<inputLength/2;i++)
      {
	 output[i]=rNormalize[i]*outputCopy[i] + iNormalize[i]*outputCopy[inputLength-i];
	 output[inputLength-i]=rNormalize[inputLength-i]*outputCopy[i] + iNormalize[inputLength-i]*outputCopy[inputLength-i];
      }

      output[0]=outputCopy[0]*rNormalize[0];
      output[inputLength/2] = outputCopy[inputLength/2]*rNormalize[inputLength/2];
      
      output.status = Object::valid;
   }

};
