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

class Mel;

//DECLARE_NODE(Mel)
NODE_INFO(Mel, "Signal", "INPUT", "OUTPUT", "INPUTLENGTH:OUTPUTLENGTH")

class Mel : public FrameOperation {
   
   int inputID;
   int inputLength;
   vector<vector<float> > filters;
   vector<int> filterStart;

public:
   Mel(string nodeName, ParameterSet params)
      : FrameOperation(nodeName, params)
      , filters(outputLength)
      , filterStart(outputLength)
   {
      inputID = addInput("INPUT");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));
   }

   ~Mel() {}

   virtual void specificInitialize()
   {
      this->FrameOperation::specificInitialize();

      float niquist = dereference_cast<int> (parameters.get("SAMPLING")) / 2.0;
      float high = dereference_cast<int> (parameters.get("HIGH"));
      float low = dereference_cast<int> (parameters.get("LOW"));
      float highMel = 1000*log(1+high/700)/log(1+1000.0/700);
      float lowMel = 1000*log(1+low/700)/log(1+1000.0/700);
      vector<int> centers(outputLength+2);
      int i;
      for (i=0;i<outputLength+2;i++)
      {
         float melCenter = lowMel + i*(highMel-lowMel)/(outputLength+1);
         centers[i] = int (floor(.5 + inputLength*700*(exp(melCenter*log(1+1000.0/700.0)/1000)-1)/niquist));
      }
      for (i=0;i<outputLength;i++)
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
      
      int i;
      int nbFilters = filters.size();
      for (i = 0 ; i < nbFilters ; i++)
      {
         int j;
         output[i]=0;
         int filterSize = filters[i].size();
         int filtStart = filterStart[i];
         for (j=0;j<filterSize;j++)
         {
            output[i] += filters[i][j]*in[j+filtStart];
         }
      }
      
      output.status = Object::valid;
   }

};
