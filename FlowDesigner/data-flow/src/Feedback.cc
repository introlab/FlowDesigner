// Copyright (C) 1999 Jean-Marc Valin

#include "Node.h"

class Feedback;

DECLARE_NODE(Feedback)
/*Node
 *
 * @name Feedback
 * @category Flow
 * @description Feedback object with a delay of n iteration
 *
 * @input_name INPUT
 * @input_description The input object
 *
 * @output_name OUTPUT
 * @output_description The output object = input object
 *
 * @output_name DELAY
 * @output_description The delayed output of DELAY iteration
 *
 * @parameter_name DELAY
 * @parameter_description Number of iteration for the delay
 * @parameter_type int
 *
END*/


class Feedback : public Node {
protected:
   int inputID;
   int delayID;
   int outputID;
   bool insideRequest;
   int delay;

public:
   Feedback(string nodeName, ParameterSet params)
      : Node(nodeName, params)
      , insideRequest(false)
   {
      try {
         inputID = addInput("INPUT");
	 outputID=addOutput("OUTPUT");
	 delayID = addOutput("DELAY");
	 delay = dereference_cast<int> (parameters.get ("DELAY"));
      } catch (BaseException *e)
      {
         //e->print();
         throw e->add(new NodeException (NULL, "Exception caught in Feedback constructor", __FILE__, __LINE__));
      }
      
   }

   void specificInitialize()
   {
      Node::specificInitialize();
      ParameterSet req;
      req.add("LOOKBACK", ObjectRef(new Int(delay)));
      req.add("INORDER", Object::nilObject);
      inputs[inputID].node->request(inputs[inputID].outputID,req);
   }

   /*WARNING: Do not try this at home. Overriding the registerOutput() method should not be 
              done unless you REALLY know what you're doing... and I'm not even sure 
              I know what I'm doing here*/
   void registerOutput (int out) 
   {
      if (out == outputID)
      {
	 incrementOutputInitialize();
      }
   }


   /**Modified the request passing method in order to avoid strange behaviours*/
   virtual void request(int output_id, const ParameterSet &req) 
   {
      if (output_id == outputID)
	 inputs[inputID].node->request(inputs[inputID].outputID,req);
   }
      
   ObjectRef getOutput(int output_id, int count)
   {
      if (output_id == outputID)
	 return getInput(inputID, count);
      else if (output_id == delayID)
      {
	 if (count-delay < 0)
	    return Object::before_beginningObject;
	 return getInput(inputID, count-delay);
      } else {
	 throw new NodeException (this, "Output not found", __FILE__, __LINE__);
      }
   }
      
};
