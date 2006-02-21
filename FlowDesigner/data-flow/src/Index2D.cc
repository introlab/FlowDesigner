// Copyright (C) 2006 Dominic Letourneau

#include "BufferedNode.h"
#include "Buffer.h"
#include "Matrix.h"

using namespace std;

namespace FD {

class Index2D;

DECLARE_NODE(Index2D)
/*Node
 *
 * @name Index2D
 * @category Matrix
 * @description Returns the value at Matrix(index0,index1)
 *
 * @input_name INPUT
 * @input_type Matrix
 * @input_description The input matrix 
 *
 * @input_name ROW
 * @input_type int
 * @input_description Index0 value (if not specified as parameter)
 *
 * @input_name COL
 * @input_type int
 * @input_description Index1 value (if not specified as parameter)
 *
 * @output_name OUTPUT
 * @output_type any
 * @output_description Matrix element at Matrix(index0,index1)
 *
 * @parameter_name ROW
 * @parameter_type int
 * @parameter_description Index0 value
 *
 * @parameter_name COL
 * @parameter_type int
 * @parameter_description Index1 value
 *
END*/


class Index2D : public BufferedNode {
   
  int m_inputMatrixID;
  int m_rowID;
  int m_colID;
  int m_outputID;  
  int m_row;
  int m_col;

public:
  Index2D(string nodeName, ParameterSet params)
    : BufferedNode(nodeName, params)
  {
    m_inputMatrixID = addInput("INPUT");
    m_outputID = addOutput("OUTPUT");
    
    if (parameters.exist("ROW")) {
      //we are using dereference_cast to make sure the index is an integer (not converted)
      m_row = dereference_cast<int>(parameters.get("ROW"));
    } else {
      m_row = -1;
      m_rowID = addInput("ROW");
    }
    
    if (parameters.exist("COL")) {
      //we are using dereference_cast to make sure the index is an integer (not converted)
      m_col = dereference_cast<int>(parameters.get("COL"));
    } else {
      m_col = -1;
      m_colID = addInput("COL");
    }     
   }

   void calculate(int output_id, int count, Buffer &out)
   {
     RCPtr<BaseMatrix> in = getInput(m_inputMatrixID,count);

     int row, col;

               
     if (m_row == -1) {
       //index was not specified in the parameters
       ObjectRef rowValue = getInput(m_rowID, count);       
       row = dereference_cast<int>(rowValue);
     } else {
       row = m_row;
     }
     
     if (m_col == -1) {
       //index was not specified in the parameters
       ObjectRef colValue = getInput(m_colID, count);       
       col = dereference_cast<int>(colValue);
     } else {
       col = m_col;
     }
         
     if (row < 0) {
       throw new NodeException(this, "Negative row index", __FILE__, __LINE__);
     }
     
     if (col < 0) {
       throw new NodeException(this, "Negative col index", __FILE__, __LINE__);
     }

     out[count] = in->getIndex(row,col);
   }
  
      
NO_ORDER_NODE_SPEEDUP(Index2D)
};

}//namespace FD
