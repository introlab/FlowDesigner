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

#include "Pack.h"
#include "net_types.h"
#include "Vector.h"
#include "multithread.h"

//DECLARE_NODE(Pack)
NODE_INFO(Pack,"Flow", "INPUT", "OUTPUT", "")

Pack::Pack(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   outputID = addOutput("OUTPUT");
   inputID = addInput("INPUT");
}

void Pack::specificInitialize()
{
   this->Node::specificInitialize();
   output = ObjectRef (new Vector<ObjectRef>);
}

void Pack::reset()
{
   this->Node::reset();
   output = ObjectRef (new Vector<ObjectRef>);
}

ObjectRef Pack::getOutput(int output_id, int count)
{
   //cerr << "Getting output in Pack\n";
   if (output_id==outputID)
   {
      lock();
      while (processCount < count)
      {
         processCount++;
         Vector<ObjectRef> &pack = object_cast<Vector<ObjectRef> > (output);
         
         NodeInput input = inputs[inputID];
         ObjectRef inputValue = input.node->getOutput(input.outputID,processCount);
         
         pack.insert(pack.end(), inputValue);
      }
      //cerr << "Pack returning: " << output << " (" << typeid(output).name() << ")" << endl;
      return unlock_and_return(output);
   }
   else 
      throw new NodeException (this, "Pack: Unknown output id", __FILE__, __LINE__);
}
