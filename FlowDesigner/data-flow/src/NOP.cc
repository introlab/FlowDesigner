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

#include <stream.h>
#include "Node.h"

class NOP;

//DECLARE_NODE(NOP)
NODE_INFO(NOP, "General", "INPUT", "OUTPUT", "")

class NOP : public Node {
protected:
   int inputID;
   int outputID;

public:
   NOP(string nodeName, ParameterSet params)
      : Node(nodeName, params)
   {
      try {
         inputID = addInput("INPUT");
	 outputID=addOutput("OUTPUT");
      } catch (BaseException &e)
      {
         e.print();
         throw NodeException (NULL, "Exception caught in NOP constructor", __FILE__, __LINE__);
      }
      
   }

   ~NOP() 
   {
   }

/**Standard request-passing method between nodes during initialization*/
      virtual void request(int outputID, const ParameterSet &req) {inputs[inputID].node->request(outputID,req);}

   ObjectRef getOutput(int output_id, int count)
   {
      NodeInput input = inputs[inputID];
      ObjectRef inputValue = input.node->getOutput(input.outputID,count);
      return inputValue;
   }

};
