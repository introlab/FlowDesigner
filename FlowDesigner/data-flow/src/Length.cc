// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

class Length;

DECLARE_NODE(Length)
/*Node
 *
 * @name Length
 * @category Vector
 * @description Get the length of a vector
 *
 * @input_name INPUT
 * @input_description The vector input
 *
 * @output_name OUTPUT
 * @output_description The length of the vector
 * @output_type int
 *
END*/


class Length : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   Length(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);
      BaseVector &in = object_cast<BaseVector> (inputValue);
      out[count] = Int::alloc(in.size());
   }

      
};
