// Copyright (C) 1999 Jean-Marc Valin

#include "Node.h"

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

   ~Discard() 
   {
   }

   /**Propagate requests*/
   virtual void request(int outputID, const ParameterSet &req)
   {
      inputs[inputID].node->request(inputs[inputID].outputID, req);
   }

   ObjectRef getOutput(int output_id, int count)
   {
      NodeInput input = inputs[inputID];
      input.node->getOutput(input.outputID,count);
      return nilObject;
   }

};
