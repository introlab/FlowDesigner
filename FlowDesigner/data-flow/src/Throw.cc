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
#include "FlowException.h"

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
