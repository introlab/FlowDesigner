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

class Autocor;

//DECLARE_NODE(Autocor)
NODE_INFO(Autocor,"Signal:DSP", "INPUT", "OUTPUT", "INPUTLENGTH:OUTPUTLENGTH:START:END")

class Autocor : public FrameOperation {
   
   int inputID;
   int inputLength;
   int start;
   int end;

public:
   Autocor(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      inputID = addInput("INPUT");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));
      start = dereference_cast<int> (parameters.get("START"));
      end = dereference_cast<int> (parameters.get("END"));
      
      inputsCache[inputID].lookBack=1;

   }

   ~Autocor() {}

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

      for (int i=0;i<outputLength;i++)
	 output[i]=0;

      float energy=0;
      for (int i=0;i<inputLength;i++)
      {
	 energy += in[i]*in[i];}/*
	 if (i<end)
	 {
	    for (int j=start;j<=i;j++)
	       output[j-start]+=in[i]*in[i-j];
	    if (can_look_back)
	    {
	       for (int j=max(i+1,start);j<=end;j++)
		 output[j-start]+=in[i]*(*past)[i-j+inputLength]; 
	    }
	 } else {
	    for (int j=start;j<=end;j++)
	    {
	       output[j-start]+=in[i]*in[i-j];
	    }
	 }
      }
				*/
      for (int i=start;i<=end;i++)
      {
	 for (int j=i;j<inputLength;j++)
	    output[i-start]+=in[j]*in[j-i];
	 if (can_look_back)
	 {
	    for (int j=0;j<i;j++)
	       output[i-start] += in[j]*(*past)[inputLength+j-i];
	 }
      }


      for (int i=0;i<outputLength;i++)
	 output[i] /= energy;
      
      output.status = Object::valid;
   }

};
