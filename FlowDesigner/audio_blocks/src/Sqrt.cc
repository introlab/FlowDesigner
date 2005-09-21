// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>
#include "vec.h"

using namespace std;

namespace FD {

class Sqrt;

DECLARE_NODE(Sqrt)
/*Node
 *
 * @name Sqrt
 * @category DSP:Base
 * @description Square root of a vector
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input vector
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Result vector of square root
 *
END*/


class Sqrt : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   Sqrt(string nodeName, ParameterSet params)
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

      vec_sqrt(&in[0], &output[0], inputLength);
   }

NO_ORDER_NODE_SPEEDUP(Sqrt)
};

}//namespace FD
