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
#include "lpc.h"
#include <stdlib.h>
#include <math.h>
#include "FFTWrap.h"

class PS2LPC;

NODE_INFO(PS2LPC,"Signal:DSP", "INPUT", "OUTPUT", "INPUTLENGTH:OUTPUTLENGTH")

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

   }

   ~PS2LPC() 
   {
      delete [] hamming;
      delete [] rc;
      delete [] response;
      delete [] ps;
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

      FFTWrap.rfft(ps, response, SAMP_SIZE);
      
      float er=0;

      response[0] *= 1.0001;
      wld(output.begin(), response, rc, outputLength-1);
      for (int i=0;i<outputLength;i++)
        output[i] *= pow(.99,i);
   }

};
