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
#include "ObjectRef.h"
#include <math.h>

class Delay;
//DECLARE_NODE(Delay)
NODE_INFO(Delay,"Flow", "INPUT", "OUTPUT", "DELAY")

class Delay : public Node {
protected:
   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'input' input*/
   int inputID;

   /**input/output count ratio*/
   int delay;
public:
   ///Constructor, takes the name of the node and a set of parameters
   Delay(string nodeName, ParameterSet params) : Node (nodeName,params)
   {
      inputID = addInput ("INPUT");
      outputID = addOutput ("OUTPUT");
      delay = dereference_cast<int> (parameters.get("DELAY"));
   }
   
   virtual void specificInitialize()
   {
      this->Node::specificInitialize();
      ParameterSet req;
      if (delay > 0)
	 req.add("LOOKBACK", ObjectRef(new Int(delay)));
      else
	 req.add("LOOKAHEAD", ObjectRef(new Int(-delay)));
      inputs[inputID].node->request(inputs[inputID].outputID, req);

   }

   virtual void reset()
   {
      this->Node::reset();
   }

   virtual ObjectRef getOutput(int output_id, int count);
   

   void request(int outputID, const ParameterSet &req)
   {
      //cerr << "name = " << name << " this = " << this << " outputID = " << outputID << endl;   cerr << "lookahead = " << outputs[outputID].lookAhead << " lookback = " << outputs[outputID].lookBack << endl;   
      
      if (req.exist("LOOKAHEAD"))
      {
	 int look = dereference_cast<int> (req.get("LOOKAHEAD")) - delay;

	 if (look > 0)
	 {
	    ParameterSet p;
	    p.add("LOOKAHEAD", ObjectRef(new Int (look)));
	    inputs[inputID].node->request(inputs[inputID].outputID,p);
	 }
      }
      if (req.exist("LOOKBACK"))
      {
	 int look = dereference_cast<int> (req.get("LOOKBACK")) + delay;

	 if (look > 0)
	 {

	    ParameterSet p;
	    p.add("LOOKBACK", ObjectRef(new Int (look)));
	    inputs[inputID].node->request(inputs[inputID].outputID,p);
	 }
      }
      //if (req.exist("CACHEALL"))
	 
      this->Node::request(outputID,req);
      
   }
      

};

ObjectRef Delay::getOutput(int output_id, int count)
{
   NodeInput input = inputs[inputID];
   if (count-delay >= 0)
      return input.node->getOutput(input.outputID,count-delay);
   else 
      return Object::before_beginningObject;
}
