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
#include <math.h>

class CMS;

DECLARE_NODE(CMS)
/*Node
 *
 * @name CMS
 * @category Signal:DSP
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name LENGTH
 * @parameter_description No description available
 *
 * @parameter_name LOOKBACK
 * @parameter_description No description available
 *
 * @parameter_name LOOKAHEAD
 * @parameter_description No description available
 *
END*/



class CMS : public BufferedNode {

   int outputID;
   int inputID;
   int length;

   int lookAhead;
   int lookBack;
   vector<float> mean;
   float decrease;
   float norm;
   bool init;

public:
   CMS(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      , init(false)
   {
      inputID = addInput("INPUT");
      outputID = addInput("OUTPUT");
      length = dereference_cast<int> (parameters.get("LENGTH"));

      inputsCache[inputID].lookBack=dereference_cast<int> (parameters.get("LOOKBACK"));
      inputsCache[inputID].lookAhead=dereference_cast<int> (parameters.get("LOOKAHEAD"));

      lookAhead = inputsCache[inputID].lookAhead;
      lookBack = inputsCache[inputID].lookBack;

      norm = 1.0/(lookAhead+lookBack);
      decrease = pow(.999,lookAhead+lookBack);
   }

   virtual void specificInitialize()
   {
      for (int i=0;i<length;i++)
	 mean[i]=0;
      BufferedNode::specificInitialize();
   }

   virtual void reset()
   {
      for (int i=0;i<length;i++)
	 mean[i]=0;
      init=false;
      BufferedNode::reset();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      NodeInput input = inputs[inputID];
      ObjectRef inputValue = input.node->getOutput(input.outputID, count);

      if (inputValue->status != Object::valid)
      {
         out[count] = inputValue;
	 return;
      }

      Vector<float> &output = *Vector<float>::alloc(length);
      out[count] = &output;

      if (!init)
      {
	 for (int i=0;i<lookAhead;i++)
	 {
	    ObjectRef nextInputValue = input.node->getOutput(input.outputID, count+lookAhead);
	    if (nextInputValue->status == Object::valid)
	    {
	       Vector<float> &curr = object_cast<Vector<float> > (nextInputValue);
	       for (int j=0;j<length;j++)
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
      
      for (int i=0;i<length;i++)
         mean[i]*=.999;

      if (can_look_back)
      {
	 for (int i=0;i<length;i++)
	    mean[i] -= decrease*(*past)[i];
      }

      if (can_look_ahead)
      {
	 for (int i=0;i<length;i++)
	    mean[i] += (*next)[i];
      }

            
      for (int i=0;i<length;i++)
         output[i] = in[i] - norm*mean[i];

   }

};
