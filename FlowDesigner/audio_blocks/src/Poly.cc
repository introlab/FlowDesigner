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

class Poly;

DECLARE_NODE(Poly)
/*Node
 *
 * @name Poly
 * @category Signal:Base
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @input_name COEF
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
END*/


class Poly : public BufferedNode {
   
   int input1ID;
   int input2ID;
   int outputID;

public:
   Poly(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      input1ID = addInput("INPUT");
      input2ID = addInput("COEF");
      outputID = addOutput("OUTPUT");

   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef input1Value = getInput(input1ID, count);

      if (input1Value->status != Object::valid)
      {
	 out[count] = input1Value;
         return;
      }

      ObjectRef input2Value = getInput(input2ID, count);

      if (input2Value->status != Object::valid)
      {
	 out[count] = input2Value;
         return;
      }

      const Vector<float> &in1 = object_cast<Vector<float> > (input1Value);
      const Vector<float> &in2 = object_cast<Vector<float> > (input2Value);
      int inputLength = in1.size();

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;
      
      for (int i=0;i<inputLength;i++)
      {
	 float x_n = 1;
	 output[i] = 0;
	 for (int j=0;j<in2.size();j++)
	 {
	    output[i] += in2[j] * x_n;
	    x_n *= in1[i];
	 }
      }
   }

};
