// Copyright (C) 2001 Jean-Marc Valin

#include "Node.h"
#include "FlowException.h"

using namespace std;
using namespace FD;

class Throw;

DECLARE_NODE(Throw)
/*Node
 *
 * @name Throw
 * @category Flow
 * @description Throw a FlowException
 *
 * @input_name INPUT
 * @input_description The Object included in the FlowException
 *
 * @output_name OUTPUT
 * @output_description Will automatically throw a FlowException if pulled
 *
END*/


class Throw : public Node {
protected:
   int inputID;
   int outputID;

public:
   Throw(string nodeName, ParameterSet params)
      : Node(nodeName, params)
   {
      try {
	 inputID=addInput("INPUT");
	 outputID=addOutput("OUTPUT");
      } catch (BaseException *e)
      {
         throw e->add(new NodeException (NULL, "Exception caught in Throw constructor", __FILE__, __LINE__));
      }
      
   }

   ObjectRef getOutput(int output_id, int count)
   {
      //throw new FlowException(getInput(inputID, count));
      throw RCPtr<FlowException> (new FlowException(getInput(inputID, count)));
   }

};
