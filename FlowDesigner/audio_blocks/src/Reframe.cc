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
#include <math.h>

class Reframe;

DECLARE_NODE(Reframe)
/*Node
 *
 * @name Reframe
 * @category Signal:DSP
 * @description Applies a window on a frame
 *
 * @input_name INPUT
 * @input_type Vector
 * @input_description Input frame
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description Reframeed frame
 *
 * @parameter_name LENGTH
 * @parameter_type int
 * @parameter_description Length of the frames
 *
 * @parameter_name ADVANCE
 * @parameter_type int
 * @parameter_description Frame advance (offset)
 *
END*/


class Reframe : public BufferedNode {
   
   int inputID;
   int outputID;
   int length;
   int advance;
   Vector<float> buff;
   int lastPos;
   int currentCount;
public:
   Reframe(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      length = dereference_cast<int> (parameters.get("LENGTH"));
      advance = dereference_cast<int> (parameters.get("ADVANCE"));
      
      inputsCache[inputID].lookBack=1;
      inputsCache[inputID].lookAhead=1;
      lastPos=0;
      currentCount=0;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      Vector<float> &output = *Vector<float>::alloc(length);
      out[count] = &output;

      //How many can we copy from the buffer
      int nbCopy = min(lastPos, length);

      cerr << nbCopy << endl;

      //Copying from the buffer
      for (int i=0;i<nbCopy;i++)
	 output[i] = buff[i];
      //How many copied so far
      int fill = nbCopy;
      //updating the buffer
      lastPos -= advance;
      
      int buffDiscard = 0;
      if (lastPos <= 0)
      {
	 buffDiscard = -lastPos;
	 lastPos = 0;
      } else {
	 for (int i=0;i<lastPos;i++)
	    buff[i] = buff[i+advance];
      }

      while (fill != output.size())
      {
	 ObjectRef inputValue = getInput(inputID, currentCount++);
	 if (inputValue->status != Object::valid)
	 {
	    out[count] = inputValue;
	    return;
	 }
	 const Vector<float> &in = object_cast<Vector<float> > (inputValue);
	 int inputLength = in.size();
	 int newPos = lastPos+inputLength-buffDiscard;

	 cerr << newPos << endl;

	 if (newPos > buff.size())
	    buff.resize(newPos);
	 for (int i=buffDiscard;i<inputLength;i++)
	    buff[i+lastPos-buffDiscard] = in[i];
	 buffDiscard-=inputLength;
	 if (buffDiscard<0)
	    buffDiscard=0;

	 nbCopy=min(inputLength, length-fill);

	 cerr << nbCopy << endl;
	 for (int i=0;i<nbCopy;i++)
	    output[i+fill] = in[i];
	 fill += nbCopy;
	 lastPos = newPos;
      }

   }

};
