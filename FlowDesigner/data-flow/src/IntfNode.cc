// Copyright (C) 2001 Jean-Marc Valin
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include "IntfNode.h"

IntfNode::IntfNode(string nodeName, const ParameterSet &params)
   : Node(nodeName, params)
   , lookAhead(0)
   , lookBack(0)
{
   outputID = addOutput("OUTPUT");
}

void IntfNode::request(int outputID, const ParameterSet &req)
{ 
   if (req.exist("LOOKAHEAD"))
      lookAhead = max(lookAhead,dereference_cast<int> (req.get("LOOKAHEAD")));
   if (req.exist("LOOKBACK"))
      lookBack = max(lookBack,dereference_cast<int> (req.get("LOOKBACK")));
   this->Node::request(outputID,req);
}

void IntfNode::init()
{
   buff = new Buffer(lookAhead+lookBack+1);
   for (int i=0;i<lookBack;i++)
      (*buff)[i] = Object::before_beginningObject;
}

void IntfNode::specificInitialize()
{
   init();
}

void IntfNode::reset()
{
   init();
}

ObjectRef IntfNode::getOutput(int output_id, int count)
{
   return (*buff)[count];
}

void IntfNode::setValue(int count, ObjectRef val)
{
   (*buff)[count+lookBack] = val;
}
