// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

class Index;

DECLARE_NODE(Index)
/*Node
 *
 * @name Index
 * @category Vector
 * @description Returns a float value from a vector
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description The input vector 
 *
 * @input_name INDEX
 * @input_type int
 * @input_description Index value (if not specified in parameter)
 *
 * @output_name OUTPUT
 * @output_type float
 * @output_description Float at a certain index
 *
 * @parameter_name INDEX
 * @parameter_type int
 * @parameter_description Index value
 *
END*/


class Index : public BufferedNode {
   
   int inputID;
   int outputID;
   int indexID;
   RCPtr<Int> index;

public:
   Index(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");

      if (parameters.exist("INDEX"))
      {
	 index = parameters.get("INDEX");
      } else {
	 index=RCPtr<Int>(Int::alloc(-1));
	 indexID = addInput("INDEX");
      }
   }

   void calculate(int output_id, int count, Buffer &out)
   {
     RCPtr<BaseVector> in = getInput(inputID,count);

     int inputLength = in->vsize();
     int ind;

     if (index->val() == -1)
       {
	 RCPtr<Int> indexValue = getInput(indexID, count);
	 ind = indexValue->val();
      } else {
	 ind = index->val();
      }

     if (ind >= inputLength)
       throw new NodeException(this, "Index larger than vector size", __FILE__, __LINE__);
     
     if (ind < 0)
       throw new NodeException(this, "Negative index", __FILE__, __LINE__);
     
     //out[count] = Float::alloc((*in)[ind]);
     out[count] = in->index(ind);
   }
  
      
NO_ORDER_NODE_SPEEDUP(Index)
};
