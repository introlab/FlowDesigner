// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

class Select;

DECLARE_NODE(Select)
/*Node
 *
 * @name Select
 * @category Vector
 * @description Selects an index range in an input vector 
 *
 * @input_name INPUT
 * @input_type Vector
 * @input_description Input vector
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description Output vector (size = END-START+1)
 *
 * @parameter_name START
 * @parameter_type int
 * @parameter_description Start index (inculded)
 *
 * @parameter_name END
 * @parameter_type int
 * @parameter_description End index (included)
 *
END*/


class Select : public BufferedNode {
   
   int inputID;
   int outputID;

   int start;
   int end;

public:
   Select(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)

   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      
      start = dereference_cast<int>(parameters.get("START"));
      end = dereference_cast<int> (parameters.get("END"));
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
      int outputLength = end-start+1;

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;

      //cerr << "Select " << name << " " << &output << " " << count << " " << inputLength << " " << end << " " << outputLength << endl;
      if (inputLength <= end)
	 throw new NodeException(this, "Input vector too short", __FILE__, __LINE__);

      int i,j;
      for (i=start, j = 0 ;i<=end;i++,j++)
      {
         output[j]=in[i];
      }
   }

};
