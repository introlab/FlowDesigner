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

class CMS;

//DECLARE_NODE(CMS)
NODE_INFO(CMS,"Signal:DSP", "INPUT", "OUTPUT", "LENGTH:LOOKBACK:LOOKAHEAD")

   //float *i_heap = ((float *)malloc( sizeof(float) * 2048))+2047;

class CMS : public FrameOperation {
   
   int inputID;
   int inputLength;

   int lookAhead;
   int lookBack;
   float *mean;
   float decrease;
   float norm;
   bool init;
public:
   CMS(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   , init(false)
   {
      inputID = addInput("INPUT");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));

      inputsCache[inputID].lookBack=dereference_cast<int> (parameters.get("LOOKBACK"));
      inputsCache[inputID].lookAhead=dereference_cast<int> (parameters.get("LOOKAHEAD"));

      lookAhead = inputsCache[inputID].lookAhead;
      lookBack = inputsCache[inputID].lookBack;

      norm = 1.0/(lookAhead+lookBack);
      decrease = pow(.999,lookAhead+lookBack);
   }

   ~CMS() {delete [] mean;}

   virtual void specificInitialize()
   {
      mean = new float [outputLength];
      for (int i=0;i<outputLength;i++)
	 mean[i]=0;
      this->FrameOperation::specificInitialize();
      
      
   }

   virtual void reset()
   {
      for (int i=0;i<outputLength;i++)
	 mean[i]=0;
      init=false;
      this->FrameOperation::reset();
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

      if (!init)
      {
	 for (int i=0;i<lookAhead;i++)
	 {
	    ObjectRef nextInputValue = input.node->getOutput(input.outputID, count+lookAhead);
	    if (nextInputValue->status == Object::valid)
	    {
	       Vector<float> &curr = object_cast<Vector<float> > (nextInputValue);
	       for (int j=0;j<outputLength;j++)
		  mean[j] += curr[j];
	    }
	 }
	 init=true;
      }

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);

      const Vector<float> *past;
      const Vector<float> *next;
      bool can_look_back = false;
      bool can_look_ahead = false;
      if (count >= lookBack)   
      {
         ObjectRef pastInputValue = input.node->getOutput(input.outputID, count-lookBack);
         if (pastInputValue->status == Object::valid)
         {
            can_look_back=true;
            past = &object_cast<Vector<float> > (pastInputValue);
         }
      }      
      
      if (1)
      {
         ObjectRef nextInputValue = input.node->getOutput(input.outputID, count+lookAhead);
         if (nextInputValue->status == Object::valid)
         {
            can_look_ahead=true;
            next = &object_cast<Vector<float> > (nextInputValue);
         }      
      }
      
      for (int i=0;i<outputLength;i++)
         mean[i]*=.999;

      if (can_look_back)
      {
	 for (int i=0;i<outputLength;i++)
	    mean[i] -= decrease*(*past)[i];
      }

      if (can_look_ahead)
      {
	 for (int i=0;i<outputLength;i++)
	    mean[i] += (*next)[i];
      }

            
      for (int i=0;i<outputLength;i++)
         output[i] = in[i] - norm*mean[i];

      output.status = Object::valid;
   }

};
