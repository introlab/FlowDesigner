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
#include "lpc.h"
#include <stdlib.h>
#include <math.h>
#include "FFTWrap.h"
#include "misc.h"

class PS2LPC;

DECLARE_NODE(PS2LPC)
/*Node
 *
 * @name PS2LPC
 * @category Signal:DSP
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name INPUTLENGTH
 * @parameter_description No description available
 *
 * @parameter_name OUTPUTLENGTH
 * @parameter_description No description available
 *
 * @parameter_name LAG_THETA
 * @parameter_description No description available
 *
END*/


class PS2LPC : public BufferedNode {
   
   int inputID;
   int outputID;
   int inputLength;
   int outputLength;

   float *hamming;
   int SAMP_SIZE;
   int SAMP_SIZE_2;

   float *response;
   float *ps;
   float *rc;
   float *lag_window;
public:
   PS2LPC(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      outputLength = dereference_cast<int> (parameters.get("OUTPUTLENGTH"));

      SAMP_SIZE_2 = inputLength;
      SAMP_SIZE   = 2 * SAMP_SIZE_2;

      rc=new float[outputLength];
      response=new float[SAMP_SIZE];
      ps=new float[SAMP_SIZE];
      lag_window=new float[SAMP_SIZE];
      if (parameters.exist("LAG_THETA"))
      {
	 for (int i=0;i<SAMP_SIZE;i++)
	    lag_window[i]=exp(-.5*sqr(2*M_PI*i*dereference_cast<float> (parameters.get("LAG_THETA"))));
      } else {
	 for (int i=0;i<SAMP_SIZE;i++)
	    lag_window[i]=1;
      }

   }

   ~PS2LPC() 
   {
      delete [] hamming;
      delete [] rc;
      delete [] response;
      delete [] ps;
      delete [] lag_window;
   }

   virtual void specificInitialize()
   {
      this->BufferedNode::specificInitialize();
      hamming = new float[SAMP_SIZE];
      for (int i=0;i<SAMP_SIZE;i++)
         hamming[i]= 0.54 - 0.46*cos(2*M_PI*i/float(SAMP_SIZE));
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
         return;
      }
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      if (inputLength != in.size())
	 throw new NodeException(this, "Input length mismatch", __FILE__, __LINE__);

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;


      for (int i=0;i<SAMP_SIZE_2;i++)
         ps[i]=in[i];
      for (int i=SAMP_SIZE_2;i<SAMP_SIZE;i++)
         ps[i]=0.0;

      FFTWrap.irfft(ps, response, SAMP_SIZE);
      for (int i=0;i<SAMP_SIZE;i++)
	 response[i] *= lag_window[i];
      
      float er=0;

      //response[0] *= 1.0001;
      wld(output.begin(), response, rc, outputLength-1);
      /*for (int i=0;i<outputLength;i++)
        output[i] *= pow(.99,i);*/
   }

};
