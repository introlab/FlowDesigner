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

class Overlap;

//DECLARE_NODE(Overlap)
NODE_INFO(Overlap,"Maitrise", "INPUT", "OUTPUT", "INPUTLENGTH:OUTPUTLENGTH")

   //float *i_heap = ((float *)malloc( sizeof(float) * 2048))+2047;

class Overlap : public FrameOperation {
   
   int inputID;
   int inputLength;
   int filterID;
   int noncausal;
   bool continuous;

public:
   Overlap(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      inputID = addInput("INPUT");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));


 
   }

   ~Overlap() {}

   virtual void specificInitialize()
   {
      inputsCache[inputID].lookBack=1;
      inputsCache[inputID].lookAhead=1;
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
      const Vector<float> *next;
      bool can_look_back = false;
      bool can_look_ahead = false;
      if (count > 0)   
      {
         ObjectRef pastInputValue = input.node->getOutput(input.outputID, count-1);
         if (pastInputValue->status == Object::valid)
         {
            can_look_back=true;
            past = &object_cast<Vector<float> > (pastInputValue);
         }      
      }
      
      ObjectRef nextInputValue = input.node->getOutput(input.outputID, count+1);
      if (nextInputValue->status == Object::valid)
      {
	 can_look_ahead=true;
	 next = &object_cast<Vector<float> > (nextInputValue);
      }
      
      
      for (int i=0;i<outputLength;i++)
         output[i]=0; 
      
      int before = (outputLength-inputLength)/2;
      int after = outputLength - inputLength - before;
      int i,j;

      //cerr << before << " " << after << endl;

      if (can_look_back)
	 for (i=0, j=inputLength-before ; i < before ; i++, j++)
	    output[i]=(*past)[j];
      

      if (can_look_ahead)
      	 for (i=0, j=outputLength-after ; i < after ; i++, j++)
	    output[j]=(*next)[i];

      for (int i=0;i<inputLength;i++)
      {
	 output[i+before]=in[i];
      }
            

      output.status = Object::valid;
   }

};
