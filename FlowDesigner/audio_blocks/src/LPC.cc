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

#include <stream.h>
#include "FrameOperation.h"
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
 * @category Signal:DSP
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name OUTPUTLENGTH
 * @parameter_description No description available
 *
 * @parameter_name RADIUS
 * @parameter_description No description available
 *
 * @parameter_name LAG_THETA
 * @parameter_description No description available
 *
END*/


class LPC : public BufferedNode {
   
   int inputID;
   int outputID;
   int outputLength;
   float *r;
   float *rc;
   float radius;
   float *lag_window;

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
      r=new float[outputLength];
      rc=new float[outputLength];
      lag_window=new float[outputLength];

      if (parameters.exist("LAG_THETA"))
      {
	 for (int i=0;i<outputLength;i++)
	    lag_window[i]=exp(-.5*sqr(2*M_PI*i*dereference_cast<float> (parameters.get("LAG_THETA"))));
      } else {
	 for (int i=0;i<outputLength;i++)
	    lag_window[i]=1;
      }
   }

   ~LPC() {delete [] r; delete [] rc; delete [] lag_window;}

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
         return;
      }
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;

      autocorr(in.begin(), r, outputLength-1, in.size());
      float er=0;
      for (int i=0;i<outputLength;i++)
	 r[i] *= lag_window[i];
      //r[0] *= 1.0001;
      r[0] += 1; //just in case of a null frame
      wld(output.begin(), r, rc, outputLength-1);
      if (radius != 1)
      {
	 for (int i=0;i<outputLength;i++)
	    output[i] *= pow(radius,i);
      }
   }

};
