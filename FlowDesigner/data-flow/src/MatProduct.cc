// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "Matrix.h"

class MatProduct;

DECLARE_NODE(MatProduct)
/*Node
 *
 * @name MatProduct
 * @category Matrix
 * @description Matrix x vector product
 *
 * @input_name INPUT
 * @input_description Input vector
 * @input_type Vector<float>
 *
 * @input_name MATRIX
 * @input_description Matrix
 * @input_type Matrix<float>
 *
 * @output_name OUTPUT
 * @output_description Result
 * @output_type Vector<float>
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
	    output[i] += mat[i][j] * in[j];
      }
      
   }

};
