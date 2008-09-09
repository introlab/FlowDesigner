// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "vec.h"
#include "Matrix.h"

using namespace std;

namespace FD {

class CovarianceAccum;

DECLARE_NODE(CovarianceAccum)
/*Node
 *
 * @name CovarianceAccum
 * @category Matrix
 * @description Updates (accumulate) a covariance matrix with an observation vector
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input (observation) vector
 *
 * @input_name MATRIX
 * @input_type Matrix<float>
 * @input_description Input (covariance) matrix
 *
 * @output_name OUTPUT
 * @output_type Matrix<float>
 * @output_description Updated matrix (same object as input)
 *
END*/

class CovarianceAccum : public BufferedNode {
   
   int inputID;
   int matrixID;
   int outputID;

public:
   CovarianceAccum(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      matrixID = addInput("MATRIX");
      outputID = addOutput("OUTPUT");
      inOrder = true;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);
      ObjectRef matrixValue = getInput(matrixID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      Matrix<float> &mat = object_cast<Matrix<float> > (matrixValue);
      size_t length = in.size();
      if (length != mat.ncols() || length != mat.nrows())
	 throw new NodeException(this, "Covariance matrix must be square and have same size as input vector", 
				 __FILE__, __LINE__);

      out[count] = matrixValue;
      for (size_t i=0;i<length;i++)
      {
	 for (size_t j=i+1;j<length;j++)
	 {
	    mat[i][j] += in[i]*in[j];
	    mat[j][i] += in[i]*in[j];
	 }
	 mat[i][i] += in[i]*in[i];
      }
   }
      
};
}//namespace FD
