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

class Greater;
DECLARE_NODE(Greater)
/*Node

 * @name Greater
 * @category Logic
 * @description No description available

 * @input_name INPUT1
 * @input_description No description available

 * @input_name INPUT2
 * @input_description No description available

 * @output_name OUTPUT
 * @output_description No description available

END*/


class Greater : public Node {
protected:
   ///The ID of the 'output' output
   int outputID;

   ///The ID of the 'input1' input
   int input1ID;

   ///The ID of the 'input2' input
   int input2ID;

   ///The true ObjectRef used to return true
   ObjectRef trueObject;

   ///The false ObjectRef used to return true
   ObjectRef falseObject;

public:
   ///Constructor, takes the name of the node and a set of parameters
   Greater(string nodeName, ParameterSet params) : Node (nodeName,params)
   {
      input1ID = addInput ("INPUT1");
      input2ID = addInput ("INPUT2");
      outputID = addOutput ("OUTPUT");
      trueObject = ObjectRef (new Bool(true));
      falseObject = ObjectRef (new Bool(false));
   }
   
   virtual void specificInitialize()
   {
      this->Node::specificInitialize();
   }

   virtual ObjectRef getOutput(int output_id, int count)
      {
	 NodeInput input1 = inputs[input1ID];
	 ObjectRef input1Value = input1.node->getOutput(input1.outputID,count);
	 NodeInput input2 = inputs[input2ID];
	 ObjectRef input2Value = input2.node->getOutput(input2.outputID,count);
	 if (dereference_cast<int> (input1Value) > dereference_cast<int> (input2Value))
	    return trueObject;
	 else
	    return falseObject;
      }
   
};

