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

class Action;

DECLARE_NODE(Action)
/*Node
 *
 * @name Action
 * @category General
 * @description Pulls in order (BEFORE,INPUT,AFTER)
 *
 * @input_name INPUT
 * @input_description The input
 *
 * @input_name BEFORE
 * @input_description To be pulled before
 *
 * @input_name AFTER
 * @input_description To be pulled after
 *
 * @output_name OUTPUT
 * @output_description The output = The input
 *
END*/


class Action : public Node {
protected:
   int inputID;
   int beforeID;
   int afterID;
   int outputID;

public:
   Action(string nodeName, ParameterSet params)
      : Node(nodeName, params)
   {
      try {
         inputID = addInput("INPUT");
         beforeID = -1;
         afterID = -1;
	 outputID=addOutput("OUTPUT");
      } catch (BaseException *e)
      {
         //e->print();
         throw e->add(new NodeException (NULL, "Exception caught in Action constructor", __FILE__, __LINE__));
      }
      
   }

   int translateInput (string inputName)
   {
      
      for (int i=0; i< inputs.size(); i++) {
	 if (inputs[i].name == inputName) {
	    return i;
	 }
      }    

      if (inputName == "BEFORE")
      {
	 beforeID = addInput(inputName);
	 return beforeID;
      } else if (inputName == "AFTER")
      {
	 afterID = addInput(inputName);
	 return afterID;
      } else {
	 throw new NodeException(this,string("Unknown input in translateInput : ") + inputName, __FILE__,__LINE__);
      }
   }


   /**Standard request-passing method between nodes during initialization*/
   virtual void request(int outputID, const ParameterSet &req)
   {
      inputs[inputID].node->request(inputs[inputID].outputID,req);
      if (beforeID != -1)
	 inputs[beforeID].node->request(inputs[beforeID].outputID,req);
      if (afterID != -1)
	 inputs[afterID].node->request(inputs[afterID].outputID,req);
   }

   ObjectRef getOutput(int output_id, int count)
   {
      if (beforeID != -1)
	 getInput(beforeID,count);
      ObjectRef inputValue = getInput(inputID,count);
      if (afterID != -1)
	 getInput(afterID,count);
      return inputValue;
   }

};
