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

class SampleDelay;

DECLARE_NODE(SampleDelay)
/*Node
 *
 * @name SampleDelay
 * @category Signal:Base
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @input_name DELAY
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name LENGTH
 * @parameter_description No description available
 *
 * @parameter_name DELAY
 * @parameter_description No description available
 *
 * @parameter_name LOOKBACK
 * @parameter_description No description available
 *
 * @parameter_name LOOKAHEAD
 * @parameter_description No description available
 *
END*/


class SampleDelay : public BufferedNode {
   
   int inputID;
   int outputID;
   int delayID;
   int delay;
   int constantDelay;
   int length;

public:
   SampleDelay(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");

      delay = 0;
      length = dereference_cast<int> (parameters.get("LENGTH"));
      if (parameters.exist("DELAY"))
      {
	 delay = dereference_cast<int> (parameters.get("DELAY"));
	 constantDelay = true;
      } else {
	 if (parameters.exist("LOOKBACK"))
	    inputsCache[inputID].lookBack=dereference_cast<int> (parameters.get("LOOKBACK"));
	 if (parameters.exist("LOOKAHEAD"))
	    inputsCache[inputID].lookAhead=dereference_cast<int> (parameters.get("LOOKAHEAD"));
	 delayID = addInput("DELAY");
	 constantDelay=false;
      } 
   }


   void calculate(int output_id, int count, Buffer &out)
   {
      if (!constantDelay)
      {
	 ObjectRef delayValue = getInput(delayID, count);
	 if (delayValue->status == Object::valid)
	 {
	    delay = (object_cast<Vector<float> > (delayValue))[0];
	    //delay = dereference_cast<float> (delayValue);
	 }
      }
      
      int frameDelay = delay/length;
      int sampleDelay = delay-frameDelay*length;

      ObjectRef firstValue=Object::nilObject;
      ObjectRef secondValue=Object::nilObject;
      if (count-frameDelay-1 >=0 )
	 firstValue = getInput(inputID, count-frameDelay-1);
      if (count-frameDelay >=0 )
	 secondValue = getInput(inputID, count-frameDelay);

      Vector<float> &output = *Vector<float>::alloc(length);
      out[count] = &output;

      if (firstValue->status == Object::valid)
      {
	 const Vector<float> &in1 = object_cast<Vector<float> > (firstValue);
	 for (int i=0;i<sampleDelay;i++)
	    output[i] = in1[length-sampleDelay+i];
      } else {
	 for (int i=0;i<sampleDelay;i++)
	    output[i] = 0;
	 
      }

      if (secondValue->status == Object::valid)
      {
	 const Vector<float> &in2 = object_cast<Vector<float> > (secondValue);
	 for (int i=sampleDelay;i<length;i++)
	    output[i] = in2[i-sampleDelay];
      } else {
	 for (int i=sampleDelay;i<length;i++)
	    output[i] = 0;
      }

   }

};