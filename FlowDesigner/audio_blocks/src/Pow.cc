// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>

using namespace std;

class Pow;

DECLARE_NODE(Pow)
/*Node
 *
 * @name Pow
 * @category DSP:Base
 * @description Raises the input vector to a certain power
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input vector
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Result: INPUT.^EXP
 *
 * @parameter_name EXP
 * @parameter_type float
 * @parameter_description Exponent
 *
END*/


class Pow : public BufferedNode {
   
   int inputID;
   int outputID;
   float exponent;

public:
   Pow(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      exponent = dereference_cast<float> (parameters.get("EXP"));
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;

      for (int i=0;i<inputLength;i++)
      {
         if (in[i] > 0)
            output[i]=pow(in[i],exponent);
         else
            output[i]=0;
      }

   }

};
