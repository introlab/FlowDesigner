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
#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <stdlib.h>

class FIR;

NODE_INFO(FIR,"Signal:DSP", "INPUT:FILTER", "OUTPUT", "CONTINUOUS:NONCAUSAL")

class FIR : public BufferedNode {
   
   int inputID;
   int outputID;
   int filterID;
   int noncausal;
   bool continuous;

public:
   FIR(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");

      filterID = addInput("FILTER");

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


   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);
      ObjectRef filterValue = getInput(filterID, count);

      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
         return;
      }

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;

      for (int i=0;i<inputLength;i++)
         output[i]=0;

      if (filterValue->status != Object::valid)
      {
         return;
      }

      const Vector<float> &filter = object_cast<Vector<float> > (filterValue);

      const Vector<float> *past;
      const Vector<float> *next;
      bool can_look_back = false;
      bool can_look_ahead = false;
      if (count > 0 && continuous)   
      {
         ObjectRef pastInputValue = getInput(inputID, count-1);
         if (pastInputValue->status == Object::valid)
         {
            can_look_back=true;
            past = &object_cast<Vector<float> > (pastInputValue);
         }      
      }      
      
      if (noncausal && continuous)
      {
         ObjectRef nextInputValue = getInput(inputID, count+1);
         if (nextInputValue->status == Object::valid)
         {
            can_look_ahead=true;
            next = &object_cast<Vector<float> > (nextInputValue);
         }
      }

      int size = filter.size();
      
      //past frames
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

      //future frames
      if (can_look_ahead)
      {
         for (int i=inputLength-noncausal;i<inputLength;i++)
         {
            int j, k;
            for (j=i+noncausal-inputLength, k=0; j>= 0; j--, k++)
            {
               output[i]+=(*next)[j]*filter[k];
            }
         }
      }

      //current frames
      for (int i=0;i<inputLength;i++)
      {
         int j, k;
         j=min(i+noncausal,inputLength-1);
         k=i+noncausal-j;
         for (; j>=max(0,i+noncausal-size+1) ; j--, k++)
         {
            output[i]+=in[j]*filter[k];
         }
      }
            
   }

};
