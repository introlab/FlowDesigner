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
#include <math.h>
#include <fftw.h>
#include <rfftw.h>

class MFCC;

//DECLARE_NODE(MFCC)
NODE_INFO(MFCC, "Signal:DSP", "INPUT", "OUTPUT", "INPUTLENGTH:OUTPUTLENGTH:WINDOW:SAMPLING:LOW:HIGH:MELBANKS")

class MFCC : public FrameOperation {
   
   int inputID;
   int inputLength;
   vector<vector<float> > filters;
   vector<int> filterStart;
   rfftw_plan plan;
   vector<float> window;
   int psLength;
   int melLength;

   rfftw_plan dctPlan;
   float *inputCopy;
   float *outputCopy;
   float *tmpBuffer1;
   float *tmpBuffer2;
   float dctNormalize;

public:
   MFCC(string nodeName, ParameterSet params)
      : FrameOperation(nodeName, params)
   {
      try {
         inputID = addInput("INPUT");
         if (parameters.exist("INPUTLENGTH"))
            inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
         else inputLength = dereference_cast<int> (parameters.get("LENGTH"));
         
         psLength = inputLength/2;
         if (parameters.exist("MELBANKS"))
            melLength = dereference_cast<int> (parameters.get("MELBANKS"));
         else
            melLength = outputLength;
         
         filters.resize(outputLength);
         filterStart.resize(outputLength);
         window.resize(inputLength);
      } catch (BaseException *e)
      {
         e->print();
         throw new NodeException (NULL, "Exception caught in MFCC constructor", __FILE__, __LINE__);
      }
      
   }

   ~MFCC() 
   {
      rfftw_destroy_plan(plan);
      rfftw_destroy_plan(dctPlan);
      delete [] inputCopy;
      delete [] outputCopy;
      delete [] tmpBuffer1;
      delete [] tmpBuffer2;
   }

   virtual void specificInitialize()
   {
      int i;

      this->FrameOperation::specificInitialize();


      String type = object_cast<String> (parameters.get("WINDOW"));
      if (type == "HANNING")
      {
         for (i=0;i<inputLength;i++)
            window[i]=.5-.5*cos((2*M_PI*i)/inputLength);
      } else if (type == "HAMMING")
      {
         for (i=0;i<inputLength;i++)
            window[i]=.54-.46*cos((2*M_PI*i)/inputLength);
      } else 
      {
         throw new GeneralException("Unknown window type",__FILE__,__LINE__);
      }


      tmpBuffer1 = new float [inputLength];
      tmpBuffer2 = new float [inputLength];
      plan = rfftw_create_plan (inputLength, FFTW_FORWARD, FFTW_ESTIMATE);


      float niquist = dereference_cast<int> (parameters.get("SAMPLING")) / 2.0;
      float high = dereference_cast<int> (parameters.get("HIGH"));
      float low = dereference_cast<int> (parameters.get("LOW"));
      float highMel = 1000*log(1+high/700)/log(1+1000.0/700);
      float lowMel = 1000*log(1+low/700)/log(1+1000.0/700);
      vector<int> centers(melLength+2);

      for (i=0;i<melLength+2;i++)
      {
         float melCenter = lowMel + i*(highMel-lowMel)/(melLength+1);
         centers[i] = int (floor(.5 + psLength*700*(exp(melCenter*log(1+1000.0/700.0)/1000)-1)/niquist));
      }
      for (i=0;i<melLength;i++)
      {
         filterStart[i] = centers[i]+1;
         filters[i].resize(centers[i+2]-centers[i]-1);
         int freq, j;
         for (freq=centers[i]+1, j=0 ; freq<=centers[i+1]; freq++, j++)
         {
            filters[i][j] = (freq-centers[i])/float(centers[i+1]-centers[i]);
         }
         for (freq=centers[i+1]+1 ; freq < centers[i+2] ; freq++, j++)
         {
            filters[i][j] = (centers[i+2]-freq)/float(centers[i+2]-centers[i+1]);
         }
      }
      

      inputCopy = new float [melLength*2];
      outputCopy =new float [melLength*2];
      dctNormalize = .5/melLength;
      dctPlan = rfftw_create_plan (melLength*2, FFTW_FORWARD, FFTW_ESTIMATE);
      
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
         tmpBuffer1[i]=in[i]*window[i];

      rfftw_one (plan, tmpBuffer1, tmpBuffer2);

      tmpBuffer2[0]=tmpBuffer2[0]*tmpBuffer2[0];
      for (i=1;i<psLength;i++)
      {
         tmpBuffer2[i] = tmpBuffer2[i]*tmpBuffer2[i]
         + tmpBuffer2[inputLength-i]*tmpBuffer2[inputLength-i];
      }

      
      int nbFilters = filters.size();
      for (i = 0 ; i < nbFilters ; i++)
      {
         int j;
         tmpBuffer1[i]=0;
         int filterSize = filters[i].size();
         int filtStart = filterStart[i];
         for (j=0;j<filterSize;j++)
         {
            tmpBuffer1[i] += filters[i][j]*tmpBuffer2[j+filtStart];
         }
      }
      
      for (i=0, j=melLength*2-1 ;i<melLength ; i++, j--)
      {
         inputCopy[i]=inputCopy[j] = log(tmpBuffer1[i]+FLT_MIN);
      }

      rfftw_one(dctPlan, inputCopy, outputCopy);

      for (i=0;i<outputLength;i++)
      {
         output[i]=dctNormalize*outputCopy[i];
      }

      output.status = Object::valid;
   }

};
