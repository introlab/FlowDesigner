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
 * @description Window-type Cepstram Mean Subtraction (CMS)
 *
 * @input_name INPUT
 * @input_type Vector
 * @input_description Input frames (cepstrum)
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description CMS output frames
 *
 * @parameter_name LENGTH
 * @parameter_type int
 * @parameter_description Frame length (number of features)
 *
 * @parameter_name LOOKBACK
 * @parameter_type int
 * @parameter_description CMS window look-back (number of frames)
 *
 * @parameter_name LOOKAHEAD
 * @parameter_type int
 * @parameter_description CMS window look-ahead (number of frames)
 *
END*/

#define NEAR_ONE .99999

class CMS : public BufferedNode {

   int outputID;
   int inputID;
   int length;

   int lookAhead;
   int lookBack;
   vector<double> mean;
   float decrease;
   float norm;
   bool init;
   int accumCount;

public:
   CMS(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      , init(false)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      length = dereference_cast<int> (parameters.get("LENGTH"));

      inputsCache[inputID].lookBack=dereference_cast<int> (parameters.get("LOOKBACK"));
      inputsCache[inputID].lookAhead=dereference_cast<int> (parameters.get("LOOKAHEAD"));

      lookAhead = inputsCache[inputID].lookAhead;
      lookBack = inputsCache[inputID].lookBack;

      mean.resize(length);

      //norm = 1.0/(lookAhead+lookBack);
      norm = (1-pow(NEAR_ONE,lookAhead+lookBack+1))/(1-NEAR_ONE)/(lookAhead+lookBack+1);
      //cerr << "norm = " << norm << endl;
      decrease = pow(NEAR_ONE,lookAhead+lookBack);
      inOrder = true;
   }

   virtual void specificInitialize()
   {
      for (int i=0;i<length;i++)
	 mean[i]=0;
      accumCount=0;
      BufferedNode::specificInitialize();
   }

   virtual void reset()
   {
      for (int i=0;i<length;i++)
	 mean[i]=0;
      accumCount = 0;
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
	       accumCount++;
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
         mean[i]*=NEAR_ONE;

      if (can_look_back)
      {
	       accumCount--;
	 for (int i=0;i<length;i++)
	    mean[i] -= decrease*(*past)[i];
      }

      if (can_look_ahead)
      {
	       accumCount++;
	 for (int i=0;i<length;i++)
	    mean[i] += (*next)[i];
      }

      //cerr << mean[0] << " " << accumCount << endl;
      float normalize = 1.0/accumCount/norm;
      for (int i=0;i<length;i++)
         output[i] = in[i] - normalize*mean[i];

   }

};
