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
#include <strstream>
#include <algorithm>

class TimeMedian;

DECLARE_NODE(TimeMedian)
/*Node
 *
 * @name TimeMedian
 * @category Signal:DSP
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name LENGTH
 * @parameter_description No description available
 *
 * @parameter_name LOOKAHEAD
 * @parameter_description No description available
 *
 * @parameter_name LOOKBACK
 * @parameter_description No description available
 *
END*/


class TimeMedian : public BufferedNode {
   
   int inputID;
   int outputID;
   int length;

   int lookBack;
   int lookAhead;
   vector<vector<float> > data;

public:
   TimeMedian(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)

   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      length = dereference_cast<int> (parameters.get("LENGTH"));
      
      lookBack = dereference_cast<int> (parameters.get("LOOKBACK"));
      inputsCache[inputID].lookBack = lookBack;

      lookAhead = dereference_cast<int> (parameters.get("LOOKAHEAD"));
      inputsCache[inputID].lookAhead = lookAhead;
      
      data.resize(length);
      for (int i = 0;i<length;i++)
	 data[i].resize(lookAhead+lookBack+1);
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      Vector<float> &output = *Vector<float>::alloc(length);
      out[count] = &output;

      NodeInput input = inputs[inputID];

      int i,j;
      
      for (j=0;j<length;j++)
         output[j] = 0.0;

      int nbValid=0;

      for (i = -lookBack; i <= lookAhead ; i++)
      {
	 if (count + i < 0)
	    continue;

         ObjectRef inputValue = input.node->getOutput(input.outputID, count + i);

         if (inputValue->status != Object::valid)
            continue;

	 const Vector<float> &in = object_cast<Vector<float> > (inputValue);

	 for (int i=0;i<length;i++)
	    data[i][nbValid] = in[i];
	 nbValid++;

      }

      for (int i=0;i<length;i++)
      {
	 sort(&(data[i][0]),&(data[i][0])+nbValid);
	 if (nbValid&1)
	    output[i] = data[i][(nbValid-1)>>1];
	 else
	    output[i] = .5*(data[i][(nbValid>>1)-1]+data[i][nbValid>>1]);
      }

   }

};
