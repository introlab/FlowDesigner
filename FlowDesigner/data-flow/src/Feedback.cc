// Copyright (C) 1999 Jean-Marc Valin
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include "Node.h"

class Feedback;

DECLARE_NODE(Feedback)
/*Node
 *
 * @name Feedback
 * @category General
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
END*/


class Feedback : public Node {
protected:
   int inputID;
   int outputID;
   bool insideRequest;

public:
   Feedback(string nodeName, ParameterSet params)
      : Node(nodeName, params)
      , insideRequest(false)
   {
      try {
         inputID = addInput("INPUT");
	 outputID=addOutput("OUTPUT");
      } catch (BaseException *e)
      {
         //e->print();
         throw e->add(new NodeException (NULL, "Exception caught in Feedback constructor", __FILE__, __LINE__));
      }
      
   }

   ~Feedback() 
   {
   }

   /*Warning: overriding the initialize function should not be done unless you REALLY 
              know what you're doing... and I'm not even sure I know what 
              I'm doing here*/
   void Feedback::initialize ()
   {
      if (initialized) return;
      if (--outputInitializeCount <=0)
      {
	 
	 specificInitialize();
	 //parameters.checkUnused();
	 
	 vector<NodeInput>::iterator in;
	 
	 for (in = inputs.begin(); in < inputs.end(); in++)
	 {        
	    if (!in->node || in->outputID == -1) {
	       throw new NodeException(this, "The node is not properly connected",__FILE__,__LINE__);
	    }
	    else {
	       if (!in->node->hasOutput(in->outputID)) 
		  throw new NodeException(this, "Input node doesn't implement output", __FILE__, __LINE__);
	       try {
		  //Deliberatly not propagating the initialization, to avoid infinite recursion
		  //in->node->initialize();
	       } catch (BaseException *e)
	       {
		  throw e->add(new NodeException(this, "Exception caught in node initialization", __FILE__, __LINE__));
	       }
	    }
	 }
	 
      }
   }
      
      
   /*Warning: overriding the initialize function should not be done unless you REALLY 
              know what you're doing... and I'm not even sure I know what 
              I'm doing here*/
   void Feedback::connectToNode(unsigned int in, Node *inputNode, unsigned int out)
   {

      
      if (inputs.size() <= in) {
	 //the old method was used for adding inputs
	 throw NodeException(this, "We shouldn't be initializing this way anymore", __FILE__, __LINE__);
	 inputs.resize(in + 1);
	 inputs[in] = NodeInput(inputNode,out,string());
      }
      else {
	 //the new method was used for adding inputs
	 inputs[in].outputID = out;
	 inputs[in].node = inputNode;
	 /*Here, I'm deliberately not registering to the input node in order to avoir an 
	   initialization problem*/
	 //inputNode->registerOutput(out);
      }
   }
      

   /**Modified the request passing method in order to avoir an infinite recursion*/
   virtual void request(int outputID, const ParameterSet &req) 
   {
      //if insideRequest is true, it means we're being called recursively... the recursion must stop
      if (!insideRequest)
      {
	 insideRequest=true;
	 ParameterSet req2(req);
	 cerr << "Got request\n";
	 req2.add("INORDER", Object::nilObject);
	 inputs[inputID].node->request(inputs[inputID].outputID,req2);
	 cerr << "Sent request\n";
	 insideRequest=false;
      }
   }

      
   ObjectRef getOutput(int output_id, int count)
   {
      if (count < 0)
	 return Object::before_beginningObject;
      return getInput(inputID, count);
   }
      
};
