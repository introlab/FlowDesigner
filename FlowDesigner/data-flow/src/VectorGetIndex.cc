#ifndef _VECTORGETINDEX_CC_
#define _VECTORGETINDEX_CC_

#include "BufferedNode.h"
#include "Vector.h"
#include "Exception.h"

using namespace std;

class VectorGetIndex;

DECLARE_NODE(VectorGetIndex)

/*Node
 * @name VectorGetIndex
 * @category Vector
 * @description Change data at the INDEX of the VECTOR by the VALUE.
 *
 * @input_name VECTOR
 * @input_type Vector
 * @input_description Vector
 *
 * @input_name INDEX
 * @input_type int
 * @input_description Vector index
 *
 * @output_name OUTPUT
 * @output_type any
 * @output_description Vector element at position index
 *
END*/


class VectorGetIndex : public BufferedNode {
   
  //inputs
  int m_vectorID;
  int m_indexID;

  //outputs
  int m_outputID;

public:

   VectorGetIndex(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params) {

     //inputs
     m_vectorID = addInput("VECTOR");
     m_indexID = addInput("INDEX");

     //outputs
     m_outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out) {
     
     try {
       ObjectRef indexValue = getInput(m_indexID,count);
	   int index = dereference_cast<int>(indexValue);
       RCPtr<BaseVector> vect = getInput(m_vectorID,count);
       out[count] = vect->getIndex(index);
     }
     catch (BaseException *e) {
       char message[256];
       ObjectRef indexValue = getInput(m_indexID,count);
	   int index = dereference_cast<int>(indexValue);
       sprintf(message,"Unable to get vector index at : %i",index);
       throw e->add (new GeneralException(message, __FILE__, __LINE__));
     }
   }//calculate
  
};



#endif
