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

class Float2Vect;

DECLARE_NODE(Float2Vect)
/*Node
 *
 * @name Float2Vect
 * @category Vector
 * @description Converts float values to a vector of elements in past and future
 *
 * @input_name INPUT
 * @input_type float
 * @input_description The input float 
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description The vector
 *
 * @parameter_name LOOKAHEAD
 * @parameter_type int
 * @parameter_description Number of elements in the future
 *
 * @parameter_name LOOKBACK
 * @parameter_type int
 * @parameter_description Number of elements in the past
 *
END*/


class Float2Vect : public BufferedNode {
   
   int inputID;
   int outputID;
   int lookAhead;
   int lookBack;

public:
   Float2Vect(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      , lookAhead(0)
      , lookBack(0)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");

      if (parameters.exist("LOOKAHEAD"))
	 lookAhead = dereference_cast<int> (parameters.get("LOOKAHEAD"));
      if (parameters.exist("LOOKBACK"))
	 lookBack = dereference_cast<int> (parameters.get("LOOKBACK"));
      inputsCache[inputID].lookAhead = lookAhead;
      inputsCache[inputID].lookBack = lookBack;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      Vector<float> &output = *Vector<float>::alloc(1 + lookAhead + lookBack);
      out[count] = &output;

      for (int i=-lookBack;i<=lookAhead;i++)
      {
	 if (count+i>=0)
	 {
	    ObjectRef inputValue = getInput(inputID, count+i);
	    if (inputValue->status != Object::valid)
	    {
	       //out[count] = inputValue;
	       //return;
	       output[lookBack+i] = 0;
	    } else {
	       output[lookBack+i] = dereference_cast<float> (inputValue);
	    }
	    
	 } else {
	    //out[count] = Object::before_beginningObject;
	    //return;
	    output[lookBack+i] = 0;
	 }
      }
      
   }

      
};
