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
#include "NNetSet.h"

class NNetSetCalc;

DECLARE_NODE(NNetSetCalc)
/*Node
 *
 * @name NNetSetCalc
 * @category NNet
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @input_name ID
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


class NNetSetCalc : public BufferedNode {
   
   int inputID;
   int netInputID;
   int IDInputID;
   int outputID;
   int outputLength;

public:
   NNetSetCalc(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      netInputID = addInput("NNET");
      IDInputID = addInput("ID");
      outputID = addOutput("OUTPUT");

      outputLength = dereference_cast<int> (parameters.get("OUTPUTLENGTH"));
      

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

      ObjectRef netValue = getInput(netInputID, count);
      if (netValue->status != Object::valid)
      {
	 out[count] = netValue;
         return;
      }
      NNetSet &net = object_cast<NNetSet> (netValue);
      
      ObjectRef idValue = getInput(IDInputID, count);
      if (idValue->status != Object::valid)
      {
	 out[count] = idValue;
         return;
      }
      const Vector<float> &id = object_cast<Vector<float> > (idValue);

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;

      double tmp[inputLength];
      for (int i=0;i<inputLength;i++)
	 tmp[i]=in[i];
      double *netOut = net.calc(int(floor(id[0])), tmp);
      
      for (int i=0;i<outputLength;i++)
         output[i]=netOut[i];
   }

};
