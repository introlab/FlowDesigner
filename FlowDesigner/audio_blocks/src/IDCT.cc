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
#include <fftw.h>
#include <math.h>

class IDCT;

//DECLARE_NODE(IDCT)
NODE_INFO(IDCT, "Signal:DSP", "INPUT", "OUTPUT", "LENGTH")

class IDCT : public FrameOperation {
   
   int inputID;
   int inputLength;

   fftw_plan plan;
   fftw_complex *inputCopy;
   fftw_complex *outputCopy;
   float *rNormalize;
   float *iNormalize;

public:
   IDCT(string nodeName, ParameterSet params)
      : FrameOperation(nodeName, params)

   {
      inputID = addInput("INPUT");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));

      if (inputLength & 1) 
      {
	 throw NodeException(NULL, "IDCT only implemented for even sizes", __FILE__, __LINE__);
      }

      inputCopy = new fftw_complex [inputLength];
      outputCopy =new fftw_complex [inputLength];
      rNormalize =new float [inputLength];
      iNormalize =new float [inputLength];
      float sqrt2n=sqrt(2.0/inputLength);
      for (int i=0;i<inputLength;i++)
      {
	 rNormalize[i]=cos(M_PI*i/(2*inputLength))*sqrt2n;
	 iNormalize[i]=sin(M_PI*i/(2*inputLength))*sqrt2n;
      }
      rNormalize[0] /= sqrt(2);

      //normalize = .5/inputLength;
      plan = fftw_create_plan (inputLength, FFTW_BACKWARD, FFTW_ESTIMATE);
}

   ~IDCT() 
   {
      fftw_destroy_plan(plan);
      delete [] inputCopy;
      delete [] outputCopy;
      delete [] rNormalize;
      delete [] iNormalize;
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


      for (i=0;i<inputLength;i++)
      {
	 inputCopy[i].re = rNormalize[i]*in[i];
	 inputCopy[i].im = iNormalize[i]*in[i];
	 //inputCopy[inputLength-i] = iNormalize[i]*in[i];
      }

      fftw_one(plan, inputCopy, outputCopy);


      for (i=0, j=0 ;i<inputLength ; i+=2, j++)
         output[i]=outputCopy[j].re;

      for (i = inputLength-1; i>=0 ; i-=2, j++)
         output[i]=outputCopy[j].re;


      output.status = Object::valid;
   }

};
