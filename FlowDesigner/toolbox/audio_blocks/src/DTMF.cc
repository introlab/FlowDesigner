// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>

using namespace std;

namespace FD {

class DTMF;

DECLARE_NODE(DTMF)
/*Node
 *
 * @name DTMF
 * @category DSP:Audio
 * @description Generates a DTMF signal
 *
 * @input_name INPUT
 * @input_type Vector<int>
 * @input_description DTMF vectors (line/column, starting at 0)
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description DTMF frames
 *
 * @parameter_name LENGTH
 * @parameter_type int
 * @parameter_description Frame length
 *
 * @parameter_name SAMPLING
 * @parameter_type int
 * @parameter_description Sampling
 *
 * @parameter_name GAIN
 * @parameter_type float
 * @parameter_description Value of the gain
 *
END*/


class DTMF : public BufferedNode {
   
   int inputID;
   int outputID;
   float gain;
   int length;
   int sampling;

   bool lastDTMF;
   vector<int> last;
   vector<double> phase;
      
   vector<double> lineFreq;
   vector<double> colFreq;

public:
   DTMF(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      , last(2)
	  , phase(2,0)
	  , lineFreq(4)
	  , colFreq(4)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      gain = dereference_cast<float> (parameters.get("GAIN"));
      length = dereference_cast<int> (parameters.get("LENGTH"));
      sampling = dereference_cast<int> (parameters.get("SAMPLING"));
      inOrder=true;
      lastDTMF=false;
      lineFreq[0] = 2*M_PI*697.0/sampling;
      lineFreq[1] = 2*M_PI*770.0/sampling;
      lineFreq[2] = 2*M_PI*852.0/sampling;
      lineFreq[3] = 2*M_PI*941.0/sampling;

      colFreq[0] = 2*M_PI*1209.0/sampling;
      colFreq[1] = 2*M_PI*1336.0/sampling;
      colFreq[2] = 2*M_PI*1477.0/sampling;
      colFreq[3] = 2*M_PI*1633.0/sampling;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      Vector<float> &output = *Vector<float>::alloc(length);
      out[count] = &output;

      const Vector<int> &in = object_cast<Vector<int> > (inputValue);

      if (!lastDTMF || in[0]!=last[0] || in[1]!=last[1])
      {
	 phase[0]=0;
	 phase[1]=0;
      }
      for (int i=0;i<length;i++)
      {
	 output[i] = gain*(sin(phase[0]) + sin(phase[1]));
	 phase[0] += lineFreq[in[0]];
	 phase[1] += colFreq[in[1]];
	 if (phase[0] > 2*M_PI)
	    phase[0] -= 2*M_PI;
	 if (phase[1] > 2*M_PI)
	    phase[1] -= 2*M_PI;
      }
      lastDTMF=true;
      last[0]=in[0];
      last[1]=in[1];
   }

      
};
}//namespace FD
