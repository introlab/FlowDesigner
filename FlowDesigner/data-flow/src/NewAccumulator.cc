// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau
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
#include "Vector.h"
#include "ObjectParser.h"
#include <iostream>
#include <sstream>

class NewAccumulator;

DECLARE_NODE(NewAccumulator)
/*Node
 *
 * @name NewAccumulator
 * @category General
 * @description Creates a new Accumulator (Vector<ObjectRef>)
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description Empty accumulator
 *
END*/


/** A constant node contains a value that will never changes. */
class NewAccumulator : public Node
{

protected:

   /**The value of the constant*/
   ObjectRef value;

   /**The ID of the 'value' output*/
   int outputID;
public:

   /**Constructor, takes the name of the node and a set of parameters*/
   NewAccumulator(string nodeName, ParameterSet params)
      : Node(nodeName, params) 
   {
      outputID = addOutput("OUTPUT");
   }

   void specificInitialize()
   {
      value = new Vector<ObjectRef>;
      Node::specificInitialize();
   }

   void reset()
   {
      value = new Vector<ObjectRef>;
      Node::reset();
   }

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count)
   {
      if (output_id==outputID) return value;
      else throw new NodeException (this, "NewAccumulator: Unknown output id", __FILE__, __LINE__);
   }
};
