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

#include "Constant.h"

//DECLARE_NODE(Constant)
NODE_INFO(Constant, "General", "", "VALUE", "VALUE")

Constant::Constant(string nodeName, ParameterSet params) 
   : Node(nodeName, params) 
   , value (parameters.get("VALUE"))
{
   outputID = addOutput("VALUE");
}

void Constant::specificInitialize()
{
   this->Node::specificInitialize();
}

void Constant::reset()
{
   this->Node::reset();
}

ObjectRef Constant::getOutput(int output_id, int count)
{
   if (output_id==outputID) return value;
   else throw NodeException (this, "Constant: Unknown output id", __FILE__, __LINE__);
}
