// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>
#include "fmath.h"

using namespace std;

class Exp;

DECLARE_NODE(Exp)
/*Node
 *
 * @name Exp
 * @category DSP:Base
 * @description Computes the exponential (base-e) of a vector
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description The input of the exponential
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Result of the exponential
 *
 * @parameter_name FAST
 * @parameter_type bool
 * @parameter_description Should we use exponential approximation
 *
END*/


class Exp : public BufferedNode {
   
   int inputID;
   int outputID;
   bool fast_exp;

public:
   Exp(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      if (parameters.exist("FAST"))
	 fast_exp = dereference_cast<bool> (parameters.get("FAST"));
      else
	 fast_exp = false;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;

      if (fast_exp)
	 for (int i=0;i<inputLength;i++)
	    output[i]=fexp(in[i]);
      else
	 for (int i=0;i<inputLength;i++)
	    output[i]=exp(in[i]);
   }

};
