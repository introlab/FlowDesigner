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

#include "Node.h"
#include "Buffer.h"

class IntfNode : public Node {
   int outputID;
   RCPtr<Buffer> buff;
   int lookAhead;
   int lookBack;
public:
   IntfNode(string nodeName, const ParameterSet &params);
   void specificInitialize();
   void reset();
   ObjectRef getOutput(int output_id, int count);

   void IntfNode::request(int outputID, const ParameterSet &req);
   void init();
   void setValue(int count, ObjectRef val);
};
