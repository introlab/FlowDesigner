// Copyright (C) 2001 Locus Dialog (author: Jean-Marc Valin)

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

class ZCrossing;

DECLARE_NODE(ZCrossing)
/*Node
 *
 * @name ZCrossing
 * @category Math
 * @description Number of zero-crossing in a vector: count(v[i]*v[i+1]<0)
 *
 * @input_name INPUT
 * @input_description The input vector 
 * @input_type Vector
 *
 * @output_name OUTPUT
 * @output_description Number of zero-crossing
 * @output_type float
 *
END*/


class ZCrossing : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   ZCrossing(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
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

      float val = 0;
      for (int i=0;i<inputLength-1;i++)
      {
	 if (in[i]*in[i+i]<0)
	    val+=1;
      }
      
      out[count] = new Float (val);

      
   }

      
};
