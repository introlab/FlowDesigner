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

#include "OutputStream.h"
#include "net_types.h"

OutputStream::OutputStream(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   outputID = addOutput("OUTPUT");
   inputID = addInput("INPUT");
}

void OutputStream::specificInitialize()
{
   this->Node::specificInitialize();
}

void OutputStream::reset()
{
   this->Node::reset();
}

ObjectRef OutputStream::getOutput(int output_id, int count)
{
   if (output_id==outputID)
   {
      if (count != processCount)
      {
         NodeInput input = inputs[inputID];
         string fileName = dereference_cast<string> (input.node->getOutput(input.outputID,count));
         openedFile = ObjectRef (new OFStream());
         ofstream &tmp = dereference_cast<ofstream> (openedFile);
         tmp.open(fileName.c_str());
      }
      return openedFile;
   }
   else 
      throw NodeException (this, "OutputStream: Unknown output id", __FILE__, __LINE__);
}
