// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "Matrix.h"
#include "lapackflow.h"

class SolveLS;

DECLARE_NODE(SolveLS)
/*Node
 *
 * @name SolveLS
 * @category Matrix
 * @require LapackFlow
 * @description Solves the min[(A*x-b)**2] least square system
 *
 * @input_name A
 * @input_description The A matrix  (M x N)
 * @input_type Matrix<float>
 *
 * @input_name B
 * @input_description The b vector (M)
 * @input_type Vector<float>
 *
 * @output_name OUTPUT
 * @output_description Result X (N)
 * @output_type Vector<float>
 *
END*/


class SolveLS : public BufferedNode {
   
   int inputID;
   int matrixID;
   int outputID;

public:
   SolveLS(string nodeName, ParameterSet params)
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
      int inputLength = mat.nrows();
      int outputLength = mat.ncols();

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;
      //if (mat.ncols() != inputLength)
//	 throw new NodeException(this, "matrix columns doesn't match vector length", __FILE__, __LINE__);

      Matrix<float> mat_copy(mat, 1);
      int tmp=1;
      float in_copy[in.size()];
      for (int i=0;i<in.size();i++)
	 in_copy[i] = in[i];
      //cerr  << inputLength << " " << outputLength << endl;
      solvels_(inputLength, outputLength, tmp, &mat_copy[0][0], in_copy);
      //cerr  << inputLength << " " << outputLength << endl;
      for (int i=0;i<output.size();i++)
	 output[i] = in_copy[i];
   }

};
