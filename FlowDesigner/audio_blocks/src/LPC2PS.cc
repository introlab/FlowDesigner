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
#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <stdlib.h>
#include <math.h>
#include "FFTWrap.h"

class LPC2PS;

DECLARE_NODE(LPC2PS)
/*Node
 *
 * @name LPC2PS
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
END*/


class LPC2PS : public BufferedNode {
   
   int inputID;
   int outputID;
   int outputLength;
   float *hamming;
   int SAMP_SIZE;
   int SAMP_SIZE_2;

   float *response;
   float *ps;

public:
   LPC2PS(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");

      outputLength = dereference_cast<int> (parameters.get("OUTPUTLENGTH"));

      SAMP_SIZE_2 = outputLength;
      SAMP_SIZE   = 2 * SAMP_SIZE_2;

      response=new float[SAMP_SIZE];
      ps=new float[SAMP_SIZE];
      hamming = new float[SAMP_SIZE];
      for (int i=0;i<SAMP_SIZE;i++)
         hamming[i]= 0.54 - 0.46*cos(2*M_PI*i/float(SAMP_SIZE));

   }

      ~LPC2PS() 
      {
	 delete [] hamming;
	 delete [] response;
	 delete [] ps;
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
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;


      for (int i=0;i<min(int(in.size()),SAMP_SIZE);i++)
         response[i]=in[i];
      for (int i=in.size();i<SAMP_SIZE;i++)
         response[i]=0;

      FFTWrap.rfft(response, ps, SAMP_SIZE);
      
      ps[0]=ps[0]*ps[0];
      for (int i=1;i<SAMP_SIZE_2;i++)
         ps[i]=ps[i]*ps[i]+ps[SAMP_SIZE-i]*ps[SAMP_SIZE-i];
      for (int i=SAMP_SIZE_2;i<SAMP_SIZE;i++)
         ps[i]=0.0;
      for (int i=0;i<SAMP_SIZE_2;i++)
         output[i]=1/ps[i];

   }

};
