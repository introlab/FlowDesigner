// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "Matrix.h"
#include "lapackflow.h"

class Eig;

DECLARE_NODE(Eig)
/*Node
 *
 * @name Eig
 * @category Matrix
 * @require LapackFlow
 * @description Finds the eigenvalues and eigenvectors of a matrix (A=V*D*V')
 *
 * @input_name INPUT
 * @input_description Input matrix A (N x N)
 * @input_type Matrix
 *
 * @output_name VALUES
 * @output_description Eigenvalues D (diag N)
 * @output_type Vector
 *
 * @output_name VECTORS
 * @output_description Eigenvectors V (N x N)
 * @output_type Vector
 *
END*/


class Eig : public BufferedNode {
   
   int inputID;
   int valuesID;
   int vectorsID;

public:
   Eig(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      valuesID = addOutput("VALUES");
      vectorsID = addOutput("VECTORS");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      if (inputValue->status != Object::valid)
      {
	 (*(outputs[valuesID].buffer))[count] = inputValue;
	 (*(outputs[vectorsID].buffer))[count] = inputValue;
      }

      Matrix<float> &mat = object_cast<Matrix<float> > (inputValue);
      int size = mat.nrows();
      if (mat.ncols() != mat.nrows())
	 throw new NodeException(this, "Matrix isn't square", __FILE__, __LINE__);

      Vector<float> &eigenvalues = *Vector<float>::alloc(size);
      Matrix<float> &eigenvectors = *(new Matrix<float> (size, size));

      Matrix<float> tmp(mat);
      //Matrix<float> tmp2(mat);

      eig_(size, &tmp[0][0], &eigenvalues[0], &eigenvectors[0][0]);
      //svd_(size, size, &tmp[0][0], &tmp2[0][0], &eigenvalues[0], &eigenvectors[0][0]);

      eigenvectors.transpose();
 
      (*(outputs[valuesID].buffer))[count] = &eigenvalues;
      (*(outputs[vectorsID].buffer))[count] = &eigenvectors;

   }

};
