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
#include <math.h>

class Saturate;

DECLARE_NODE(Saturate)
/*Node

 * @name Saturate
 * @category Effects
 * @description No description available

 * @input_name INPUT
 * @input_description No description available

 * @output_name OUTPUT
 * @output_description No description available

 * @parameter_name LENGTH
 * @parameter_description No description available

 * @parameter_name SATURATION
 * @parameter_description No description available

 * @parameter_name THRESHOLD
 * @parameter_description No description available

END*/


class Saturate : public FrameOperation {
   
   int inputID;
   int inputLength;
   enum Type {HARD, TANH, ATAN, SOFT4};

   float threshold;
   Type saturation;

public:
   Saturate(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      inputID = addInput("INPUT");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));

      threshold = dereference_cast<float> (parameters.get("THRESHOLD"));
      
      if (parameters.exist("INPUTLENGTH"))
      {
	 String satur = object_cast<String> (parameters.get("SATURATION"));
	 if (satur == "hard")
	    saturation = HARD;
	 else if (satur == "tanh")
	    saturation = TANH;
	 else if (satur == "atan")
	    saturation = ATAN;
	 else if (satur == "soft4")
	    saturation = SOFT4;
	 else saturation = HARD;
      } else saturation = HARD;
   }

   ~Saturate() {}

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
      
      switch (saturation)
      {
	 case HARD:
	    for (int i=0;i<outputLength;i++)
	    {
	       output[i] = in[i];
	       if (output[i] < -threshold)
		  output[i] = -threshold;
	       else if (output[i] > threshold)
		  output[i] = threshold;
	    }
	    break;
	 case ATAN:
	    for (int i=0;i<outputLength;i++)
	    {
	       output[i] = ((2/M_PI)*threshold) * atan(in[i]/threshold);
	    }
	    break;
	 case TANH:
	    for (int i=0;i<outputLength;i++)
	    {
	       output[i] = threshold * tanh(in[i]/threshold);
	    }
	    break;
      }
      
      output.status = Object::valid;
   }

};
