// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "Matrix.h"

using namespace std;

namespace FD {

class OffsetMatrix;

DECLARE_NODE(OffsetMatrix)
/*Node
 *
 * @name OffsetMatrix
 * @category DSP:Manip
 * @description Returns a matrix of frames with offset
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input frame
 *
 * @output_name OUTPUT
 * @output_type Matrix<float>
 * @output_description Matrix (ready for SVD, ...)
 *
 * @parameter_name COLUMNS
 * @parameter_type int
 * @parameter_description Number of columns (subframe length)
 *
 * @parameter_name ROWS
 * @parameter_type int
 * @parameter_description Number of rows (number of offsets)
 *
END*/


class OffsetMatrix : public BufferedNode {
   
   int inputID;
   int outputID;
   int cols;
   int rows;

public:
   OffsetMatrix(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      cols = dereference_cast<int> (parameters.get("COLUMNS"));
      rows = dereference_cast<int> (parameters.get("ROWS"));
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Matrix<float> *output = new Matrix<float>(rows,cols);
      out[count] = output;

      if (cols+rows-1>inputLength)
      {
	 cerr << cols << " " << rows << " " << inputLength << endl;
	 throw new NodeException(this, "Frame too small", __FILE__, __LINE__);
      }
      
      for (int i=0;i<rows;i++)
	 for (int j=0;j<cols;j++)
	    (*output)(i,j)=in[i+j];
      
   }

      
};
}//namespace FD
