// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

using namespace std;
using namespace FD;

class Index2Vector;

DECLARE_NODE(Index2Vector)
/*Node
 *
 * @name Index2Vector
 * @category DSP:Manip
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name LENGTH
 * @parameter_description No description available
 *
END*/


class Index2Vector : public BufferedNode {
   
   int inputID;
   int outputID;
   int length;

public:
   Index2Vector(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      length = dereference_cast<int> (parameters.get("LENGTH"));
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int index = int(in[0]);
      if (index >= length || index < 0)
	 throw new NodeException(this, "Index out of range", __FILE__, __LINE__);
      //int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(length);
      out[count] = &output;

      
      for (int i=0;i<length;i++)
      {
         output[i]=0;
      }
      output[index] = 1;
   }

};
