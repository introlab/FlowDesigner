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

#include "Save.h"
#include "net_types.h"
#include "Object.h"

//DECLARE_NODE(Save)
NODE_INFO(Save, "IO", "OBJECT:STREAM", "OUTPUT", "")

Save::Save(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   outputID = addOutput("OUTPUT");
   streamInputID = addInput("STREAM");
   objectInputID = addInput("OBJECT");
}

void Save::specificInitialize()
{
   this->Node::specificInitialize();
}

void Save::reset()
{
   this->Node::reset();
}

ObjectRef Save::getOutput(int output_id, int count)
{
   if (output_id==outputID)
   {
      if (count != processCount)
      {
         NodeInput streamInput = inputs[streamInputID];
         ObjectRef streamValue = streamInput.node->getOutput(streamInput.outputID,count);
         OStream &stream = object_cast<OStream> (streamValue);

         NodeInput objectInput = inputs[objectInputID];
         ObjectRef objectValue = objectInput.node->getOutput(objectInput.outputID,count);
         Object &object = *objectValue;
         stream << object << endl;
         stream.flush();
         return objectValue;
      } else {
         NodeInput objectInput = inputs[objectInputID];
         ObjectRef objectValue = objectInput.node->getOutput(objectInput.outputID,count);
         return objectValue;         
      }
      //return ObjectRef(new Object(Object::nil));
   }
   else 
      throw new NodeException (this, "Save: Unknown output id", __FILE__, __LINE__);
}
