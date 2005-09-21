// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "Matrix.h"
#include "lapackflow.h"

using namespace std;

namespace FD {

class LinSolve;

DECLARE_NODE(LinSolve)
/*Node
 *
 * @name LinSolve
 * @category Matrix
 * @require LapackFlow
 * @description Solves the A*x=b linear system
 *
 * @input_name A
 * @input_description The A matrix (N x N
 * @input_type Matrix<float>
 *
 * @input_name B
 * @input_description The b vector (N)
 * @input_type Vector<float>
 *
 * @output_name OUTPUT
 * @output_description Result X (N)
 * @output_type Vector<float>
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


      Vector<float> &in = object_cast<Vector<float> > (inputValue);
      Matrix<float> &mat = object_cast<Matrix<float> > (matrixValue);
      int inputLength = in.size();
      int outputLength = mat.nrows();
      if (mat.ncols() != inputLength)
	 throw new NodeException(this, "matrix columns doesn't match vector length", __FILE__, __LINE__);

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;

      int tmp=1;
      solve_(inputLength, tmp, &mat[0][0], &in[0], &output[0]);
      //solve (inputLength, 1, &mat[0][0], &output[0]);
   }

};
}//namespace FD
