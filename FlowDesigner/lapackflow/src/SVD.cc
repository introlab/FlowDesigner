// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "Matrix.h"
#include "lapackflow.h"

class SVD;

DECLARE_NODE(SVD)
/*Node
 *
 * @name SVD
 * @category Matrix
 * @description Finds the eigenvalues and eigenvectors of a matrix (A=U*SIGMA*V')
 *
 * @input_name INPUT
 * @input_description Input matrix A
 * @input_type Matrix
 *
 * @output_name U
 * @output_description Left singular vectors U
 * @output_type Matrix
 *
 * @output_name SIGMA
 * @output_description Singular values SIGMA
 * @output_type Vector
 *
 * @output_name V
 * @output_description Right singular vectors V
 * @output_type Matrix
 *
END*/


class SVD : public BufferedNode {
   
   int inputID;
   int valuesID;
   int lvectorsID;
   int rvectorsID;

public:
   SVD(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      valuesID = addOutput("SIGMA");
      lvectorsID = addOutput("U");
      rvectorsID = addOutput("V");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      if (inputValue->status != Object::valid)
      {
	 (*(outputs[valuesID].buffer))[count] = inputValue;
	 (*(outputs[lvectorsID].buffer))[count] = inputValue;
	 (*(outputs[rvectorsID].buffer))[count] = inputValue;
      }

      Matrix<float> &mat = object_cast<Matrix<float> > (inputValue);
      int rows = mat.nrows();
      int cols = mat.ncols();

      Vector<float> &svalues = *Vector<float>::alloc(min(rows, cols));
      Matrix<float> &lvectors = *(new Matrix<float> (rows, rows));
      Matrix<float> &rvectors = *(new Matrix<float> (cols, cols));

      Matrix<float> tmp(mat, 1);

      //eig_(size, &tmp[0][0], &eigenvalues[0], &eigenvectors[0][0]);
      svd_(rows, cols, &tmp[0][0], &lvectors[0][0], &svalues[0], &rvectors[0][0]);

      lvectors.transpose();
 
      (*(outputs[valuesID].buffer))[count] = &svalues;
      (*(outputs[lvectorsID].buffer))[count] = &lvectors;
      (*(outputs[rvectorsID].buffer))[count] = &rvectors;

   }

};