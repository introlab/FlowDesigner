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
#include "FFNet.h"

class NNetExecRecurrent;

DECLARE_NODE(NNetExecRecurrent)
/*Node
 *
 * @name NNetExecRecurrent
 * @category NNet
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @input_name NNET
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name OUTPUTLENGTH
 * @parameter_description No description available
 *
END*/


class NNetExecRecurrent : public BufferedNode {
   
   int inputID;
   int netInputID;
   double *prev_output;
   int outputID;
   int outputLength;
public:
   NNetExecRecurrent(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inOrder = true;
      inputID = addInput("INPUT");
      netInputID = addInput("NNET");
      outputID = addOutput("OUTPUT");

      outputLength = dereference_cast<int> (parameters.get("OUTPUTLENGTH"));
      prev_output=new double [outputLength];
   }

   ~NNetExecRecurrent() 
   {
      delete [] prev_output;
   }

   virtual void specificInitialize()
   {
      for (int i=0;i<outputLength;i++)
	 prev_output[i]=0;

      this->BufferedNode::specificInitialize();
   }

   virtual void reset()
   {
      for (int i=0;i<outputLength;i++)
	 prev_output[i]=0;

      this->BufferedNode::reset();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef netValue = getInput(netInputID, count);
      if (netValue->status != Object::valid)
      {
	 out[count] = netValue;
         return;
      }

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


      FFNet &net = object_cast<FFNet> (netValue);
      
      //int classID = vq.getClassID(in.begin());
      //const vector<float> &mean = vq[classID];
      double tmp[in.size()+outputLength];
      for (int i=0;i<in.size();i++)
	 tmp[i]=in[i];
      for (int i=0;i<outputLength;i++)
	 tmp[i+in.size()] = prev_output[i];
      double *netOut = net.calc(tmp);
      for (int i=0;i<outputLength;i++)
	 prev_output[i]=netOut[i];
      
      for (int i=0;i<outputLength;i++)
         output[i]=netOut[i];
       
   }

};
