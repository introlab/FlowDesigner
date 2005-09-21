//Copyright (C) 2001 Locus Dialog 
//Author: Jean-Marc Valin


#include "Node.h"
#include "Matrix.h"
#include "ObjectParser.h"
#include <iostream>
#include <sstream>

using namespace std;

namespace FD {

class DCMatrix;

DECLARE_NODE(DCMatrix)
/*Node
 *
 * @name DCMatrix
 * @category Matrix
 * @description Creates a matrix of identical values
 *
 * @output_name OUTPUT
 * @output_type Matrix<float>
 * @output_description The matrix
 *
 * @parameter_name ROWS
 * @parameter_type int
 * @parameter_description Number of rows
 *
 * @parameter_name COLUMNS
 * @parameter_type int
 * @parameter_description Number of columns
 *
 * @parameter_name VALUE
 * @parameter_type float
 * @parameter_description Value of each element
 *
END*/


/** A constant node contains a value that will never changes. */
class DCMatrix : public Node
{

protected:

   /**The value of the constant*/
   ObjectRef value;

   /**The ID of the 'value' output*/
   int outputID;
public:

   /**Constructor, takes the name of the node and a set of parameters*/
   DCMatrix(string nodeName, ParameterSet params)
      : Node(nodeName, params) 
      //, value (parameters.get("VALUE"))
   {
      outputID = addOutput("OUTPUT");
      int rows = dereference_cast<int> (parameters.get("ROWS"));
      int cols = dereference_cast<int> (parameters.get("COLUMNS"));
      
      value = ObjectRef(new Matrix<float>(rows, cols));
      Matrix<float> &val = object_cast<Matrix<float> > (value);
      float value = dereference_cast<float> (parameters.get("VALUE"));;
      for (int i=0;i<rows;i++)
	 for (int j=0;j<cols;j++)
	    val[i][j]=value;
   }

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count)
   {
      if (output_id==outputID) return value;
      else throw new NodeException (this, "DCMatrix: Unknown output id", __FILE__, __LINE__);
   }

protected:
   /**Default constructor, should not be used*/
   DCMatrix() {throw new GeneralException("DCMatrix copy constructor should not be called",__FILE__,__LINE__);}

};
}//namespace FD
