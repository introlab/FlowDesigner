// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "lpc.h"
#include <stdlib.h>
#include <math.h>
#include "misc.h"

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

class LPC;

DECLARE_NODE(LPC)
/*Node
 *
 * @name LPC
 * @category DSP:Adaptive
 * @description Performs LPC (Linear predictive coefficient) analysis
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input (audio) vector
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description LPC coefficients (including a[0]=1)
 *
 * @parameter_name OUTPUTLENGTH
 * @parameter_type int
 * @parameter_description Number of LPC coefficients (order = OUTPUTLENGTH-1)
 *
 * @parameter_name RADIUS
 * @parameter_type float
 * @parameter_description Maximum radius of the poles (used for bandwidth expansion)
 *
 * @parameter_name LAG_THETA
 * @parameter_type float
 * @parameter_description Minimum resonnance bandwidth allowed (with lag-windowing, approximative)
 *
END*/


class LPC : public BufferedNode {
   
   int inputID;
   int outputID;
   int outputLength;
   vector<float> r;
   vector<float> rc;
   float radius;
   vector<float> lag_window;

public:
   LPC(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");

      outputLength = dereference_cast<int> (parameters.get("OUTPUTLENGTH"));

      if (parameters.exist("RADIUS"))
	 radius = dereference_cast<float> (parameters.get("RADIUS"));
      else radius=1;
      r.resize(outputLength);
      rc.resize(outputLength);
      lag_window.resize(outputLength);

      if (parameters.exist("LAG_THETA"))
      {
	 for (int i=0;i<outputLength;i++)
	    lag_window[i]=exp(-.5*sqr(2*M_PI*i*dereference_cast<float> (parameters.get("LAG_THETA"))));
      } else {
	 for (int i=0;i<outputLength;i++)
	    lag_window[i]=1;
      }
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;

      autocorr(&in[0], &r[0], outputLength-1, in.size());
      float er=0;
      for (int i=0;i<outputLength;i++)
	 r[i] *= lag_window[i];
      r[0] *= 1.0001;
      r[0] += 1; //just in case of a null frame
      wld(&output[0], &r[0], &rc[0], outputLength-1);
      if (radius != 1)
      {
	 for (int i=0;i<outputLength;i++)
	    output[i] *= pow(radius,i);
      }
   }

};
