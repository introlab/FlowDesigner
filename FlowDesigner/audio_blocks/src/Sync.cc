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

class Sync;
DECLARE_NODE(Sync)

class Sync : public Node {
protected:
   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'input' input*/
   int inputID;

   /**input/output count ratio*/
   float ratio;
public:
   ///Constructor, takes the name of the node and a set of parameters
   Sync(string nodeName, ParameterSet params) : Node (nodeName,params)
   {
      inputID = addInput ("INPUT");
      outputID = addOutput ("OUTPUT");
      ratio = dereference_cast<float> (parameters.get("RATIO"));
   }
   
   virtual void specificInitialize()
   {
      this->Node::specificInitialize();
   }

   virtual void reset()
   {
      this->Node::reset();
   }

   virtual ObjectRef getOutput(int output_id, int count);
   
};

ObjectRef Sync::getOutput(int output_id, int count)
{
   NodeInput input = inputs[inputID];
   return input.node->getOutput(input.outputID,int(floor(.5+count*ratio)));
}
