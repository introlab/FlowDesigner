// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "Matrix.h"

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

class MarkovProb;

DECLARE_NODE(MarkovProb)
/*Node
 *
 * @name MarkovProb
 * @category HMM
 * @description Calculates the Markov chain probability
 *
 * @input_name INPUT
 * @input_description State probability
 *
 * @input_name MATRIX
 * @input_description Transition probability matrix
 *
 * @output_name OUTPUT
 * @output_description A posteriori probability
 *
END*/


class MarkovProb : public BufferedNode {
   
   int inputID;
   int outputID;
   int matrixID;

public:
   MarkovProb(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      matrixID = addInput("MATRIX");
      outputID = addOutput("OUTPUT");
      inOrder=1;
      outputs[outputID].lookBack += 1;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
         return;
      }
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int length = in.size();

      ObjectRef matrixValue = getInput(matrixID, count);
      if (matrixValue->status != Object::valid)
      {
	 out[count] = matrixValue;
	 return;
      }
      const Matrix<float> &mat = object_cast<Matrix<float> > (matrixValue);
      if (mat.nrows() != mat.nrows() || mat.nrows() != length)
	 throw new NodeException(this, "Transition matrix has wrong size", __FILE__, __LINE__);

      Vector<float> &output = *Vector<float>::alloc(length);
      out[count] = &output;

      if (count > 0)
      {
	 //ObjectRef pastValue = getOutput(outputID, count-1);
	 /*if (pastValue->status != Object::valid)
	 {
	    out[count] = pastValue;
	    return;
	    }*/
	 //const Vector<float> &past = object_cast<Vector<float> > (pastValue);
	 const Vector<float> &past = object_cast<Vector<float> > (out[count-1]);

	 if (past.size() != length)
	    throw new NodeException(this, "Number of states changed... that's odd", __FILE__, __LINE__);

	 for (int i=0;i<length;i++)
	 {
	    output[i] = 0;
	    for (int j=0;j<length;j++)
	       output[i] += mat[i][j] * past[j];
	 }
      } else {
	 for (int i=0;i<length;i++)
	    output[i] = 1;
      }
      float sum=10*FLT_MIN;
      for (int i=0;i<length;i++)
      {
	 output[i] *= in[i];
	 sum += output[i];
      }
     
      sum = 1.0/sum;
      for (int i=0;i<length;i++)
      {
	 //cerr << output[i] << " ";
	 output[i] += FLT_MIN;
	 output[i] *= sum;
      }
      //cerr << endl;
      for (int i=0;i<length;i++)
      {
	 //cerr << output[i] << " ";
      }
      //cerr << endl;
   }

};
