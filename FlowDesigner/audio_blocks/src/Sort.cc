// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "vec.h"
#include <algorithm>

using namespace std;

class Sort;

DECLARE_NODE(Sort)
/*Node
 *
 * @name Sort
 * @category DSP:Base
 * @description Sorts an input vector
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input vector
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Sorted output vector
 *
END*/

class Sort : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   Sort(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;

      for (int i=0;i<inputLength;i++)
         output[i]=in[i];

      sort (&output[0], &output[inputLength]);
   }

   NO_ORDER_NODE_SPEEDUP(Sort)
      
};
