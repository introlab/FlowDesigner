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

class IterWall;

DECLARE_NODE(IterWall)
/*Node
 *
 * @name IterWall
 * @category Flow
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
END*/


class IterWall : public Node {
protected:
   int inputID;
   int outputID;
   int iter;
      ObjectRef value;
      bool calculated;

public:
   IterWall(string nodeName, ParameterSet params)
      : Node(nodeName, params)
   {
      try {
         inputID = addInput("INPUT");
	 outputID=addOutput("OUTPUT");
	 if (parameters.exist("ITER"))
	    iter = dereference_cast<int> (parameters.get("ITER"));
	 else
	    iter = 0;
      } catch (BaseException *e)
      {
         throw e->add(new NodeException (NULL, "Exception caught in IterWall constructor", 
					 __FILE__, __LINE__));
      }
      
   }

   virtual void specificInitialize()
   {
      Node::specificInitialize();
      calculated = false;
   }

   virtual void reset()
   {
      Node::reset();
      calculated = false;
   }

   ObjectRef getOutput(int output_id, int count)
   {
      if (!calculated)
      {
	 value = getInput(inputID, iter);
      }
      return value;
   }

};
