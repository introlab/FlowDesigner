// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "Matrix.h"
#include "lapackflow.h"

class LinSolve;

DECLARE_NODE(LinSolve)
/*Node
 *
 * @name LinSolve
 * @category Vector
 * @description Solves the Ax=b linear system
 *
 * @input_name A
 * @input_description The A matrix
 * @input_type Matrix
 *
 * @input_name B
 * @input_description The b vector
 * @input_type Vector
 *
 * @output_name OUTPUT
 * @output_description Result X
 * @output_type Vector
 *
END*/


class LinSolve : public BufferedNode {
   
   int inputID;
   int matrixID;
   int outputID;

public:
   LinSolve(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("B");
      matrixID = addInput("A");
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

      Vector<float> &in = object_cast<Vector<float> > (inputValue);
      Matrix<float> &mat = object_cast<Matrix<float> > (matrixValue);
      int inputLength = in.size();
      int outputLength = mat.nrows();
      if (mat.ncols() != inputLength)
	 throw new NodeException(this, "matrix columns doesn't match vector length", __FILE__, __LINE__);

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;

      for (int i=0;i<inputLength;i++)
	 output[i]=in[i];
      solve (inputLength, 1, &mat[0][0], &output[0]);
   }

};
