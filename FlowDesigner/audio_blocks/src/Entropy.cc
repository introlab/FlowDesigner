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
#include <strstream.h>
#include <values.h>

class Entropy;

//DECLARE_NODE(Entropy)
NODE_INFO(Entropy,"Signal", "INPUT", "OUTPUT", "INPUTLENGTH:OUTPUTLENGTH:LOOKAHEAD:LOOKBACK")

class Entropy : public FrameOperation {
   
   int inputID;
   int inputLength;

   int numberFrames;
   vector<Vector<float> *> frames;
   vector<float> min;

public:
   Entropy(string nodeName, ParameterSet params)
      : FrameOperation(nodeName, params)

   {
      inputID = addInput("INPUT");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));
      
      //if (parameters.exist("LOOKAHEAD"))
         inputsCache[inputID].lookAhead = dereference_cast<int> (parameters.get("LOOKAHEAD"));
      //if (parameters.exist("LOOKBACK"))
         inputsCache[inputID].lookBack = dereference_cast<int> (parameters.get("LOOKBACK"));
      
      numberFrames=inputsCache[inputID].lookBack+inputsCache[inputID].lookAhead+1;
      frames.resize(numberFrames);
      min.resize(numberFrames);
   }

   ~Entropy() {}

   virtual void specificInitialize()
   {
      this->FrameOperation::specificInitialize();
      
   }

   static inline float dist (float *in1, float *in2, int length)
   {
      int i;
      float sum=0;
      for (i=0;i<length;i++)
      {
         float tmp;
         tmp=in1[i]-in2[i];
         sum += tmp*tmp;
      }
      return sum;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      Vector<float> &output = object_cast<Vector<float> > (out[count]);
      if (count < inputsCache[inputID].lookBack)
      {
         output.status = Object::before_beginning;
         return;
      }
      NodeInput input = inputs[inputID];

      int i,j;

      for (i = -inputsCache[inputID].lookBack, j=0; i <= inputsCache[inputID].lookAhead ; i++, j++)
      {
         Ptr<Vector<float> > inputValue = input.node->getOutput(input.outputID, count + i);
         //ObjectRef inputValue = input.node->getOutput(input.outputID, count + i);
         if (inputValue->status != Object::valid)
         {
            output.status = inputValue->status;
            return;
         }
         frames[j] = inputValue.get();
         //frames[j] = object_ptr_cast<Vector<float> *> (inputValue);
      }      
      
      //cerr << numberFrames << " " << (*(frames[0]))[0] << " " ; 

      for (i=0;i<numberFrames;i++)
         min[i]=FLT_MAX;
      for (i=0;i<numberFrames;i++)
            for (j=i+1;j<numberFrames;j++)
            {
               float tmp=dist(frames[i]->begin(), frames[j]->begin(), inputLength);
               if (tmp < min[i]) min[i]=tmp;
               if (tmp < min[j]) min[j]=tmp;
            }
      float accum=0;
      for (i=0;i<numberFrames;i++)
      {
         //cerr <<  min[i] << " ";
         accum += min[i];
      }
      output[0] = accum/numberFrames;
      //cerr << output[0] << endl;


      output.status = Object::valid;
   }

};
