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

#include "Node2.h"

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
 * @output_name DELAY
 * @output_description No description available
 *
 * @parameter_name DELAY
 * @parameter_description No description available
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

   ~Feedback() 
   {
   }

   /*Warning: overriding the initialize function should not be done unless you REALLY 
              know what you're doing... and I'm not even sure I know what 
              I'm doing here*/
   void Feedback::initialize ()
   {
      if (initialized) return;
      //FIXME: This is a big kludge, it won't work if more than 1 link is connected to "OUTPUT"
      if (1 || --outputInitializeCount <=0)
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
		  in->node->initialize();
	       } catch (BaseException *e)
	       {
		  throw e->add(new NodeException(this, "Exception caught in node initialization", __FILE__, __LINE__));
	       }
	    }
	 }
	 
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
	 //error
      }
   }
      
};
