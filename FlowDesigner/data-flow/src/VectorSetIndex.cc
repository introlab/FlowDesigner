#ifndef _VECTORSETINDEX_CC_
#define _VECTORSETINDEX_CC_

#include "BufferedNode.h"
#include "Vector.h"


class VectorSetIndex;

DECLARE_NODE(VectorSetIndex)

/*Node
 * @name VectorSetIndex
 * @category Vector
 * @description Change data at the INDEX of the VECTOR by the VALUE.
 *
 * @input_name VECTOR
 * @input_type Vector<float>
 * @input_description Vector
 *
 * @input_name VALUE
 * @input_type float
 * @input_description value to put in Vector[index]
 *
 * @input_name INDEX
 * @input_type int
 * @input_description Vector index
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Float transformation to integer
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

     Vector<float> &my_vect = object_cast<Vector<float> >(getInput(m_vectorID,count));
     int index = dereference_cast<int>(getInput(m_indexID,count));
     float value = dereference_cast<float>(getInput(m_valueID,count));
     
     
     if (index >= my_vect.size()) {
       throw new GeneralException("Index out of bound!",__FILE__,__LINE__);
     }
     else {
       
       //create a new vector with the same size
       Vector<float> *vect = Vector<float>::alloc(my_vect.size());

       //copying vector
       for (int i = 0; i < my_vect.size(); i++) {
	 (*vect)[i] = my_vect[i];
       }

       //changing value at desired index
       (*vect)[index] = value;


       //output new vector
       out[count] = ObjectRef(vect);

     }

   }//calculate
  
};



#endif
