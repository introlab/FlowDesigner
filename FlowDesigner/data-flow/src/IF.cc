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

class IF;

DECLARE_NODE(IF)
/*Node
 *
 * @name IF
 * @category Logic
 * @description Takes a branch or another depending on a condition (Bool value).
 *
 * @input_name COND
 * @input_description The condition for the if statement
 *
 * @input_name THEN
 * @input_description What to do if the condition is true
 *
 * @input_name ELSE
 * @input_description What to do if the condition is false
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
END*/


class IF : public Node {
protected:
   int inputID;
   int thenID;
   int elseID;
   int outputID;

public:
   IF(string nodeName, ParameterSet params)
      : Node(nodeName, params)
   {
      try {
         inputID = addInput("COND");
         thenID = addInput("THEN");
         elseID = addInput("ELSE");
	 outputID=addOutput("OUTPUT");
      } catch (BaseException *e)
      {
         throw e->add(new NodeException (NULL, "Exception caught in IF constructor", __FILE__, __LINE__));
      }
      
   }

/**Standard request-passing method between nodes during initialization*/
   virtual void request(int outputID, const ParameterSet &req) 
   {
      inputs[inputID].node->request(inputs[inputID].outputID,req);
      inputs[thenID].node->request(inputs[thenID].outputID,req);
      inputs[elseID].node->request(inputs[elseID].outputID,req);
   }

   ObjectRef getOutput(int output_id, int count)
   {
      //NodeInput input = inputs[inputID];
      //ObjectRef inputValue = input.node->getOutput(input.outputID,count);
      if (dereference_cast<bool> (getInput(inputID,count)))
	 return getInput(thenID,count);
      else 
	 return getInput(elseID,count);
   }

};
