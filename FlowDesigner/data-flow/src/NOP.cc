// Copyright (C) 1999 Jean-Marc Valin

#include "Node.h"

class NOP;

DECLARE_NODE(NOP)
/*Node
 *
 * @name NOP
 * @category General
 * @description Pass Through (no operation)
 *
 * @input_name INPUT
 * @input_description The input
 *
 * @output_name OUTPUT
 * @output_description The output = The input
 *
END*/


class NOP : public Node {
protected:
   int inputID;
   int outputID;

public:
   NOP(string nodeName, ParameterSet params)
      : Node(nodeName, params)
   {
      try {
         inputID = addInput("INPUT");
	 outputID=addOutput("OUTPUT");
      } catch (BaseException *e)
      {
         throw e->add(new NodeException (NULL, "Exception caught in NOP constructor", __FILE__, __LINE__));
      }
      
   }

   /**Standard request-passing method between nodes during initialization*/
   virtual void request(int outputID, const ParameterSet &req)
   {
      inputs[inputID].node->request(inputs[inputID].outputID,req);
   }

   ObjectRef getOutput(int output_id, int count)
   {
      ObjectRef inputValue = getInput(inputID,count);
      return inputValue;
   }

};
