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
#include "fmath.h"

class Exp;

DECLARE_NODE(Exp)
/*Node
 *
 * @name Exp
 * @category Signal:Base
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name FAST
 * @parameter_description Should we use exponential approximation
 *
END*/


class Exp : public BufferedNode {
   
   int inputID;
   int outputID;
   bool fast_exp;

public:
   Exp(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      if (parameters.exist("FAST"))
	 fast_exp = dereference_cast<bool> (parameters.get("FAST"));
      else
	 fast_exp = false;
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

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;

      if (fast_exp)
	 for (int i=0;i<inputLength;i++)
	    output[i]=fexp(in[i]);
      else
	 for (int i=0;i<inputLength;i++)
	    output[i]=exp(in[i]);
   }

};
