// Copyright (C) 1999 Jean-Marc Valin

#include "Node.h"

class Action;

DECLARE_NODE(Action)
/*Node
 *
 * @name Action
 * @category General
 * @description Pulls in order the inputs: BEFORE, INPUT and AFTER. 
 *
 * @input_name INPUT
 * @input_description The input
 *
 * @input_name BEFORE
 * @input_description To be pulled before
 *
 * @input_name AFTER
 * @input_description To be pulled after
 *
 * @output_name OUTPUT
 * @output_description The output = The input
 *
END*/


class Action : public Node {
protected:
   int inputID;
   int beforeID;
   int afterID;
   int outputID;

public:
   Action(string nodeName, ParameterSet params)
      : Node(nodeName, params)
   {
      try {
         inputID = addInput("INPUT");
         beforeID = -1;
         afterID = -1;
	 outputID=addOutput("OUTPUT");
      } catch (BaseException *e)
      {
         //e->print();
         throw e->add(new NodeException (NULL, "Exception caught in Action constructor", __FILE__, __LINE__));
      }
      
   }

   int translateInput (string inputName)
   {
      
      for (unsigned int i=0; i< inputs.size(); i++) {
	 if (inputs[i].name == inputName) {
	    return i;
	 }
      }    

      if (inputName == "BEFORE")
      {
	 beforeID = addInput(inputName);
	 return beforeID;
      } else if (inputName == "AFTER")
      {
	 afterID = addInput(inputName);
	 return afterID;
      } else {
	 throw new NodeException(this,string("Unknown input in translateInput : ") + inputName, __FILE__,__LINE__);
      }
   }


   /**Standard request-passing method between nodes during initialization*/
   virtual void request(int outputID, const ParameterSet &req)
   {
      inputs[inputID].node->request(inputs[inputID].outputID,req);
      if (beforeID != -1)
	 inputs[beforeID].node->request(inputs[beforeID].outputID,req);
      if (afterID != -1)
	 inputs[afterID].node->request(inputs[afterID].outputID,req);
   }

   ObjectRef getOutput(int output_id, int count)
   {
      if (beforeID != -1)
	 getInput(beforeID,count);
      ObjectRef inputValue = getInput(inputID,count);
      if (afterID != -1)
	 getInput(afterID,count);
      return inputValue;
   }

};
