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
#include "Matrix.h"

class MatProduct;

DECLARE_NODE(MatProduct)
/*Node
 *
 * @name MatProduct
 * @category Signal:Base
 * @description Matrix x vector product
 *
 * @input_name INPUT
 * @input_description Input vector
 *
 * @input_name MATRIX
 * @input_description Matrix
 *
 * @output_name OUTPUT
 * @output_description Result
 *
END*/


class MatProduct : public BufferedNode {
   
   int inputID;
   int matrixID;
   int outputID;

public:
   MatProduct(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      matrixID = addInput("MATRIX");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);
      ObjectRef matrixValue = getInput(matrixID, count);

      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
         return;
      }
      if (matrixValue->status != Object::valid)
      {
	 out[count] = matrixValue;
         return;
      }

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      const Matrix<float> &mat = object_cast<Matrix<float> > (matrixValue);
      int inputLength = in.size();
      int outputLength = mat.nrows();
      if (mat.ncols() != inputLength)
	 throw new NodeException(this, "matrix columns doesn't match vector length", __FILE__, __LINE__);

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;

      for (int i=0;i<outputLength;i++)
      {
	 output[i] = 0;
	 for (int j=0;j<inputLength;j++)
	    output[i] += mat[i][j] + in[j];
      }
      
   }

};
