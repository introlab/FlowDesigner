#ifndef _ISNIL_CC_
#define _ISNIL_CC_

#include "BufferedNode.h"

using namespace std;

class isNil;

DECLARE_NODE(isNil)

/*Node
 * @name isNil
 * @category Logic
 * @description No description available
 *
 * @input_name INPUT
 * @input_description Float value
 *
 * @output_name OUTPUT
 * @output_type bool
 * @output_description Float transformation to integer
 *
END*/


class isNil : public BufferedNode {
   
  //inputs
  int inputID;

  //outputs
  int outputID;

public:

   isNil(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params) {

     //inputs
     inputID = addInput("INPUT");
     //outputs
     outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out) {

     if (getInput(inputID,count)->isNil()) {
       out[count] = ObjectRef(Bool::alloc(true));
     }
     else {
       out[count] = ObjectRef(Bool::alloc(false));
     }

   }//calculate
  
};



#endif
