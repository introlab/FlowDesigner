// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>
#include "fmath.h"

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

class Log;

DECLARE_NODE(Log)
/*Node
 *
 * @name Log
 * @category DSP:Base
 * @description Computes the natural logarithm of a vector
 *
 * @input_name INPUT
 * @input_type Vector
 * @input_description The input of the log
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description Result of the log
 *
 * @parameter_name FAST
 * @parameter_type bool
 * @parameter_description Should we use fast log approximation?
 *
END*/


class Log : public BufferedNode {
   
   int inputID;
   int outputID;
   bool fast_log;
      
public:
   Log(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      if (parameters.exist("FAST"))
	 fast_log = dereference_cast<bool> (parameters.get("FAST"));
      else
	 fast_log = false;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;

      if (fast_log)
	 for (int i=0;i<inputLength;i++)
	    output[i]=flog(in[i]+FLT_MIN);
      else
	 for (int i=0;i<inputLength;i++)
	    output[i]=log(in[i]+FLT_MIN);
      
   }
      
};
