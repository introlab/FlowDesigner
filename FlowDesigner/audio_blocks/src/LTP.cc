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
#include <math.h>

class LTP;

//DECLARE_NODE(LTP)
NODE_INFO(LTP,"Maitrise", "INPUT", "OUTPUT", "INPUTLENGTH:OUTPUTLENGTH:START:END:GAIN_FACTOR")

class LTP : public FrameOperation {
   
   int inputID;
   int inputLength;
   int start;
   int end;
   float factor;

public:
   LTP(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      inputID = addInput("INPUT");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));
      start = dereference_cast<int> (parameters.get("START"));
      end = dereference_cast<int> (parameters.get("END"));
      if (parameters.exist("GAIN_FACTOR"))
	 factor = dereference_cast<float> (parameters.get("GAIN_FACTOR"));
      else 
	 factor = 1;
   }

   ~LTP() {}

   virtual void specificInitialize()
   {
      inputsCache[inputID].lookBack=1;
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

      const Vector<float> *past;
      bool can_look_back = false;
      if (count > 0)   
      {
         ObjectRef pastInputValue = input.node->getOutput(input.outputID, count-1);
         if (pastInputValue->status == Object::valid)
         {
            can_look_back=true;
            past = &object_cast<Vector<float> > (pastInputValue);
         }      
      }      

      float best=-FLT_MAX;
      float best_gain=0;
      int best_T=0;

      for (int lag=start;lag<=end;lag++)
      {
	 float corr=0;
	 float energy=0;
	 for (int i=lag;i<inputLength;i++)
	 {
	    corr += in[i]*in[i-lag];
	    energy += in[i-lag]*in[i-lag];
	 }
	 if (can_look_back)
	    for (int i=0;i<lag;i++)
	    {
	       corr += in[i] * (*past)[inputLength+i-lag];
	       energy += (*past)[inputLength+i-lag] * (*past)[inputLength+i-lag];
	    } 
	 float score = /*corr*/corr/energy;
	 //cout << corr/energy << endl;
	 if (score > best)
	 {
	    best = score;
	    best_T = lag;
	    best_gain = corr/energy;
	 }
      }

      if (best_gain > 1.2)
	 best_gain = 1.2;
      if (best_gain < -.2)
	 best_gain = -.2;
      //cout << endl;
      output[0] = best_gain * factor;
      output[1] = best_T;

      output.status = Object::valid;
   }

};
