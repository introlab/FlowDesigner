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

class IterCount;
//DECLARE_NODE(IterCount)
NODE_INFO(IterCount,"Logic", "", "OUTPUT", "")

class IterCount : public Node {
protected:
   ///The ID of the 'output' output
   int outputID;

public:
   ///Constructor, takes the name of the node and a set of parameters
   IterCount(string nodeName, ParameterSet params) : Node (nodeName,params)
   {
      outputID = addOutput ("OUTPUT");
      //trueObject = ObjectRef (new Int(Zero));
      //falseObject = ObjectRef (new Bool(false));
   }
   
   virtual void specificInitialize()
   {
      this->Node::specificInitialize();
   }

   virtual ObjectRef getOutput(int output_id, int count) 
   {
      return ObjectRef(new Int(count));
   }
   
};
