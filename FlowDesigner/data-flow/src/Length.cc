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
 * @input_type Vector<float>
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


      int sz;
      if (typeid(*inputValue) == typeid(Vector<ObjectRef>))
      {
	 //cerr << "Vector<ObjectRef>\n";
	 sz = object_cast<Vector<ObjectRef> > (inputValue).size();
      } else if (typeid(*inputValue) == typeid(Vector<float>))
      {
	 //cerr << "Vector<float>\n";
	 sz = object_cast<Vector<float> > (inputValue).size();
      } else {
	 //cerr << "error!!!\n";
	 throw new NodeException(this, "Unknown input type", __FILE__, __LINE__);
      }

      out[count] = Int::alloc(sz);

   }

      
};
