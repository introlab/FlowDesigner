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

#include "UnPack.h"
#include "net_types.h"
#include "Vector.h"
#include "multithread.h"

UnPack::UnPack(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   outputID = addOutput("OUTPUT");
   inputID = addInput("INPUT");
}

void UnPack::specificInitialize()
{
   this->Node::specificInitialize();
}

void UnPack::reset()
{
   this->Node::reset();
}

ObjectRef UnPack::getOutput(int output_id, int count)
{
   //cerr << "Getting output in UnPack\n";
   if (output_id==outputID)
   {
      processCount=count;
      
      NodeInput input = inputs[inputID];
      ObjectRef inputValue = input.node->getOutput(input.outputID,count);
      
      Vector<ObjectRef> &packed = object_cast <Vector<ObjectRef> > (inputValue);
      if (count < packed.size())
         return packed[count];
      else
         return Object::past_endObject;
   }
   else 
      throw NodeException (this, "UnPack: Unknown output id", __FILE__, __LINE__);
}
