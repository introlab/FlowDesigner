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

class FIR;

//DECLARE_NODE(FIR)
NODE_INFO(FIR,"Signal", "INPUT:FILTER", "OUTPUT", "LENGTH:CONTINUOUS:NONCAUSAL")

   //float *i_heap = ((float *)malloc( sizeof(float) * 2048))+2047;

class FIR : public FrameOperation {
   
   int inputID;
   int inputLength;
   int filterID;
   int noncausal;
   bool continuous;

public:
   FIR(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      inputID = addInput("INPUT");
      filterID = addInput("FILTER");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));

      if (parameters.exist("CONTINUOUS"))
         continuous=true;
      else
         continuous=false;

      if (parameters.exist("NONCAUSAL"))
         noncausal = dereference_cast<int> (parameters.get("NONCAUSAL"));
      else 
         noncausal=0;

      if (continuous)
         inputsCache[inputID].lookBack=1;
      if (noncausal && continuous)
         inputsCache[inputID].lookAhead=1;

   }

   ~FIR() {}

   virtual void specificInitialize()
   {
      this->FrameOperation::specificInitialize();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      NodeInput input = inputs[inputID];
      ObjectRef inputValue = input.node->getOutput(input.outputID, count);

      NodeInput filterInput = inputs[filterID];
      ObjectRef filterValue = filterInput.node->getOutput(filterInput.outputID, count);

      Vector<float> &output = object_cast<Vector<float> > (out[count]);
      if (inputValue->status != Object::valid)
      {
         output.status = inputValue->status;
         return;
      }
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      const Vector<float> &filter = object_cast<Vector<float> > (filterValue);
      const Vector<float> *past;
      const Vector<float> *next;
      bool can_look_back = false;
      bool can_look_ahead = false;
      if (count > 0 && continuous)   
      {
         ObjectRef pastInputValue = input.node->getOutput(input.outputID, count-1);
         if (pastInputValue->status == Object::valid)
         {
            can_look_back=true;
            past = &object_cast<Vector<float> > (pastInputValue);
         }      
      }      
      
      if (noncausal && continuous)
      {
         ObjectRef nextInputValue = input.node->getOutput(input.outputID, count+1);
         if (nextInputValue->status == Object::valid)
         {
            can_look_ahead=true;
            next = &object_cast<Vector<float> > (nextInputValue);
         }      
      }

      int size = filter.size();
      
      for (int i=0;i<outputLength;i++)
         output[i]=0;

      if (can_look_back)
      {
         for (int i=0;i<size-noncausal-1;i++)
         {
            int j, k;
            for (j=inputLength-1, k=1+i+noncausal; k<size ; j--, k++)
            {
               output[i]+=(*past)[j]*filter[k];
            }
         }
      }

      if (can_look_ahead)
      {
         for (int i=outputLength-noncausal;i<outputLength;i++)
         {
            int j, k;
            for (j=i+noncausal-outputLength, k=0; j>= 0; j--, k++)
            {
               output[i]+=(*next)[j]*filter[k];
            }
         }
      }

      for (int i=0;i<outputLength;i++)
      {
         int j, k;
         j=min(i+noncausal,inputLength-1);
         k=i+noncausal-j;
         for (; j>=max(0,i+noncausal-size+1) ; j--, k++)
         {
            output[i]+=in[j]*filter[k];
         }
      }
            

      output.status = Object::valid;
   }

};
