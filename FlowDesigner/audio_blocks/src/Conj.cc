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
#include <complex>

class Conj;

DECLARE_NODE(Conj)
/*Node
 *
 * @name Conj
 * @category Signal:Base
 * @description Computes the complex conjugate of a vector
 *
 * @input_name INPUT
 * @input_type Vector
 * @input_description Input vector
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description Conjugate vector
 *
END*/


class Conj : public BufferedNode {
   
   int inputID;
   int outputID;
   float gain;

public:
   Conj(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
         return;
      }
      const Vector<complex<float> > &in = object_cast<Vector<complex<float> > > (inputValue);
      int inputLength = in.size();

      Vector<complex<float> > &output = *Vector<complex<float> >::alloc(inputLength);
      out[count] = &output;

      for (int i=0;i<inputLength;i++)
	 output[i] = conj(in[i]);
      
   }

      
};
