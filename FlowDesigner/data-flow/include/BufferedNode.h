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

#ifndef BUFFERED_NODE_H
#define BUFFERED_NODE_H

#include "Node.h"

class BufferedNode : public Node {
protected:
public:
   /**Constructor, takes the name of the node and a set of parameters*/
   BufferedNode(string nodeName, const ParameterSet &params);
   
   /**Destructor*/
   virtual ~BufferedNode() {}

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count) = 0; 

   /**Standard request-passing method between nodes during initialization*/
   virtual void request(const ParameterSet &req) {}

   /**Class specific initialization routine.
      Each class will call its subclass specificInitialize() method*/
   virtual void specificInitialize();

};

#endif
