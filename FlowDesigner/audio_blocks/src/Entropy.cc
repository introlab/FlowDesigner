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

#include "FrameOperation.h"
#include "Buffer.h"
#include "Vector.h"
#include <strstream>
#include <values.h>
#include <math.h>

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

class Entropy;

DECLARE_NODE(Entropy)
/*Node

 * @name Entropy
 * @category Signal:DSP
 * @description No description available

 * @input_name INPUT
 * @input_description No description available

 * @output_name OUTPUT
 * @output_description No description available

 * @parameter_name INPUTLENGTH
 * @parameter_description No description available

 * @parameter_name OUTPUTLENGTH
 * @parameter_description No description available

 * @parameter_name LOOKAHEAD
 * @parameter_description No description available

 * @parameter_name LOOKBACK
 * @parameter_description No description available

END*/


class Entropy : public FrameOperation {
   
   int inputID;
   int inputLength;

public:
   Entropy(string nodeName, ParameterSet params)
      : FrameOperation(nodeName, params)

   {
      inputID = addInput("INPUT");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));
   }

   ~Entropy() {}

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
      
      float s2=0;
      float entr=0;
      for (int i=0;i<inputLength;i++)
      {
         s2+=in[i]*in[i];
      }
      s2 = 1/s2;

      for (int i=0;i<inputLength;i++)
      {
	 if (in[i] != 0)
	    entr -= s2*in[i]*in[i] * log(s2*in[i]*in[i]);
      }
      cout << entr << endl;
      output[0] = entr;

      output.status = Object::valid;
   }

};
