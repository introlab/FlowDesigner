//This file is a copy used for static linking of Overflow applications. If it is
//part of the Overflow code base, then it is released under the LGPL license.
//For more information, see the COPYING file in the Overflow source directory.

// Copyright (C) 1999 Jean-Marc Valin

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
	    output[lookBack+i] = dereference_cast<float> (inputValue);
	    
	 } else {
	    output[lookBack+i] = 0;
	 }
      }
      
   }

      
NO_ORDER_NODE_SPEEDUP(Float2Vect)
};
