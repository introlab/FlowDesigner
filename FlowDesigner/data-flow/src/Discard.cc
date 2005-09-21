// Copyright (C) 1999 Jean-Marc Valin

#include "Node.h"

using namespace std;

namespace FD {

class Discard;

DECLARE_NODE(Discard)
/*Node
 *
 * @name Discard
 * @category General
 * @description Discards the object pulled
 *
 * @input_name INPUT
 * @input_description The input object
 *
 * @output_name OUTPUT
 * @output_type NilObject
 * @output_description Always return a NilObject
 *
END*/


class Discard : public Node {
protected:
   int inputID;
   int outputID;

public:
   Discard(string nodeName, ParameterSet params)
      : Node(nodeName, params)
   {
      try {
         inputID = addInput("INPUT");
	 outputID=addOutput("OUTPUT");
      } catch (BaseException *e)
      {
         //e->print();
         throw e->add(new NodeException (NULL, "Exception caught in Discard constructor", __FILE__, __LINE__));
      }
      
   }
   
   int translateInput (string inputName)
   {
      for (unsigned int i=0; i< inputs.size(); i++) {
         if (inputs[i].name == inputName) {
            return i;
         }
      }  
      return addInput(inputName);
   }

   ObjectRef getOutput(int output_id, int count)
   {
      for (unsigned int i=0; i< inputs.size(); i++)
      {
        getInput(i, count);
      }
      
      return nilObject;
   }

};

}//namespace FD
