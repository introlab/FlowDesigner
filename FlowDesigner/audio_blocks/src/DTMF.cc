// Copyright (C) 1999 Jean-Marc Valin
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>

class DTMF;

DECLARE_NODE(DTMF)
/*Node
 *
 * @name DTMF
 * @category Signal:Audio
 * @description Generates a DTMF signal
 *
 * @input_name INPUT
 * @input_type Vector
 * @input_description DTMF vectors (line/column, starting at 0)
 *
 * @output_name OUTPUT
 * @output_type Vector
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
      , lineFreq(4)
      , colFreq(4)
      , phase(2,0)
      , last(2)
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

      if (inputValue->status != Object::valid)
      {
	 lastDTMF=false;
	 for (int i=0;i<length;i++)
	    output[i]=0;
         return;
      }
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
