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
 * @input_type bool
 *
 * @input_name THEN
 * @input_description What to do if the condition is true
 *
 * @input_name ELSE
 * @input_description What to do if the condition is false
 *
 * @output_name OUTPUT
 * @output_description The object from THEN or ELSE depending on COND
 *
 * @parameter_name PULL_ANYWAY
 * @parameter_type bool
 * @parameter_description If true, the IF statement pulls also on the branch not taken
 *
END*/


class IF : public Node {
protected:
   int inputID;
   int thenID;
   int elseID;
   int outputID;
   bool pullAnyway;

public:
   IF(string nodeName, ParameterSet params)
      : Node(nodeName, params)
   {
      try {
         inputID = addInput("COND");
         thenID = addInput("THEN");
         elseID = addInput("ELSE");
	 outputID=addOutput("OUTPUT");
	 if (parameters.exist("PULL_ANYWAY"))
	    pullAnyway = dereference_cast<bool> (parameters.get("PULL_ANYWAY"));
	 else
	    pullAnyway = false;
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
      {
	 if (pullAnyway)
	    getInput(elseID,count);
	 return getInput(thenID,count);
      } else {
	 if (pullAnyway)
	    getInput(thenID,count);
	 return getInput(elseID,count);
      }
   }

};
