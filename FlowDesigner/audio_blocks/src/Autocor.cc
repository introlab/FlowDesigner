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

class Autocor;

DECLARE_NODE(Autocor)
/*Node
 *
 * @name Autocor
 * @category Signal:DSP
 * @description Computes the autocorrelation of an input vector with (START <= lag <= END)
 *
 * @input_name INPUT
 * @input_type Vector
 * @input_description Input vector
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description Autocorrelation vector
 *
 * @parameter_name START
 * @parameter_type int
 * @parameter_description Smallest lag offset (included)
 *
 * @parameter_name END
 * @parameter_type int
 * @parameter_description Largest lag offset (included)
 *
 * @parameter_name CONTINUOUS
 * @parameter_type bool
 * @parameter_description Use the previous frame also (cross-correlation) (default false)
 *
 * @parameter_name NORMALIZE
 * @parameter_type bool
 * @parameter_description Energy normalization (default false)
 *
 * @parameter_name NORMALIZE2
 * @parameter_type bool
 * @parameter_description Normalize by subtracting the value at lag/2 (default false)
 *
END*/


class Autocor : public BufferedNode {
   
   int inputID;
   int outputID;
   int start;
   int end;
   bool continuous;
   bool normalize;
   bool normalize2;

public:
   Autocor(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");

      start = dereference_cast<int> (parameters.get("START"));
      end = dereference_cast<int> (parameters.get("END"));
      
      if (parameters.exist("CONTINUOUS"))
      {
	 if (dereference_cast<bool> (parameters.get("CONTINUOUS")))
	    continuous = true;
	 else 
	    continuous = false;
      } else {
	 continuous = false;
      }

      if (parameters.exist("NORMALIZE"))
      {
	 if (dereference_cast<bool> (parameters.get("NORMALIZE")))
	    normalize = true;
	 else 
	    normalize = false;
      } else 
	 normalize = false;

      if (parameters.exist("NORMALIZE2"))
      {
	 if (dereference_cast<bool> (parameters.get("NORMALIZE2")))
	    normalize2 = true;
	 else 
	    normalize2 = false;
      } else 
	 normalize2 = false;
      

      if (continuous)
	 inputsCache[inputID].lookBack=1;
      
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
      int outputLength = end-start+1;

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;

      const Vector<float> *past;
      bool can_look_back = false;
      if (continuous && count > 0)   
      {
         ObjectRef pastInputValue =  getInput(inputID, count-1);
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
	 energy += in[i]*in[i];
      }
				
      for (int i=start;i<=end;i++)
      {
	 for (int j=i;j<inputLength;j++)
	    output[i-start]+=in[j]*in[j-i];
	 if (can_look_back && continuous)
	 {
	    for (int j=0;j<i;j++)
	       output[i-start] += in[j]*(*past)[inputLength+j-i];
	 }
      }

      if (normalize)
      {
	 float e_1 = 1/energy;
	 for (int i=0;i<outputLength;i++)
	 {
	    output[i] *= e_1;
	 }
      }

      if (normalize2)
      {
	 float tmp[outputLength];
	 for (int i=0;i<outputLength;i++)
	 {
	    tmp[i] = output[i];
	    output[i] -= max(0.0f,tmp[i>>1]);
	 }
      }
   }
      
};
