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

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <stdlib.h>

class Overlap;

DECLARE_NODE(Overlap)
/*Node

 * @name Overlap
 * @category Signal:DSP
 * @description No description available

 * @input_name INPUT
 * @input_description No description available

 * @output_name OUTPUT
 * @output_description No description available

 * @parameter_name OUTPUTLENGTH
 * @parameter_description No description available

END*/


class Overlap : public BufferedNode {
   
   int inputID;
   int outputID;
   int filterID;
   int outputLength;

public:
   Overlap(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      outputLength = dereference_cast<int> (parameters.get("OUTPUTLENGTH"));
   }

   virtual void specificInitialize()
   {
      inputsCache[inputID].lookBack=1;
      inputsCache[inputID].lookAhead=1;
      this->BufferedNode::specificInitialize();
      
      
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
         return;
      }
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;

      const Vector<float> *past;
      const Vector<float> *next;
      bool can_look_back = false;
      bool can_look_ahead = false;
      if (count > 0)   
      {
         ObjectRef pastInputValue = getInput(inputID, count-1);
         if (pastInputValue->status == Object::valid)
         {
            can_look_back=true;
            past = &object_cast<Vector<float> > (pastInputValue);
         }      
      }
      
      ObjectRef nextInputValue = getInput(inputID, count+1);
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
   }

};
