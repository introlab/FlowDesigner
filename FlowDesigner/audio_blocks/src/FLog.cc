// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>
#include "fmath.h"

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

class FLog;

DECLARE_NODE(FLog)
/*Node
 *
 * @name FLog
 * @category DSP:Base
 * @description Computes the natural logarithm of a vector using a *rough* appriximation (only 17 MSB used)
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description The input of the log
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Result of the log
 *
END*/


class FLog : public BufferedNode {
   
   int inputID;
   int outputID;
      
public:
   FLog(string nodeName, ParameterSet params)
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

      fflogv(&in[0], &output[0], inputLength);
      
   }
      
   NO_ORDER_NODE_SPEEDUP(FLog)
};
