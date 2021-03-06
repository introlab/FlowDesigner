#ifndef _VECTORSETINDEX_CC_
#define _VECTORSETINDEX_CC_

#include "BufferedNode.h"
#include "Vector.h"
#include "Exception.h"

using namespace std;

namespace FD {

class VectorSetIndex;

DECLARE_NODE(VectorSetIndex)

/*Node
 * @name VectorSetIndex
 * @category Vector
 * @description Change data at the INDEX of the VECTOR by the VALUE.
 *
 * @input_name VECTOR
 * @input_type Vector
 * @input_description Vector
 *
 * @input_name VALUE
 * @input_type any
 * @input_description value to put in Vector[index]
 *
 * @input_name INDEX
 * @input_type int
 * @input_description Vector index
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description The same vector as the input vector
 *
END*/


class VectorSetIndex : public BufferedNode {
   
  //inputs
  int m_vectorID;
  int m_valueID;
  int m_indexID;

  //outputs
  int m_outputID;

public:

   VectorSetIndex(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params) {

     //inputs
     m_vectorID = addInput("VECTOR");
     m_valueID = addInput("VALUE");
     m_indexID = addInput("INDEX");

     //outputs
     m_outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out) {

     try {
       ObjectRef indexValue = getInput(m_indexID,count);

	   int index = dereference_cast<int>(indexValue);
	       
       RCPtr<BaseVector> input_vect = getInput(m_vectorID,count);

       //should clone the vector before modifying it
       RCPtr<BaseVector> vect = input_vect->clone();

       ObjectRef value = getInput(m_valueID,count);
       
       vect->setIndex(index,value);
       
       out[count] = vect;
     }
     catch(BaseException *e) {

       char message[256];
       ObjectRef indexValue = getInput(m_indexID,count);
	   int index = dereference_cast<int>(indexValue);
       sprintf(message,"unable to set vector index at : %i",index);
       throw e->add(new GeneralException(message,__FILE__,__LINE__));
     }

   }//calculate
  
};

}//namespace FD

#endif
