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

#include "BufferedNode.h"

BufferedNode::BufferedNode(string nodeName, const ParameterSet &params)
   : Node(nodeName, params)
{}

int BufferedNode::addInput (const string &input_name)
{
   /*handle input buffers here*/
   return Node::addInput (input_name);
}

int BufferedNode::addOutput(const string &output_name)
{
   /*handle output buffers here*/
   return Node::addOutput(output_name);
}

void BufferedNode::specificInitialize()
{
   //cerr << "FrameOperation initialize...\n";
   this->Node::specificInitialize();
   if (cacheAll)
   {
      output = ObjectRef(new GrowingFrameBuffer<float> (outputLength, outputLookAhead+outputLookBack+1));
   } else {
      output = ObjectRef(new RotatingFrameBuffer<float> (outputLength, outputLookAhead+outputLookBack+1));
   }
}

void BufferedNode::reset()
{
   this->Node::reset();
   if (cacheAll)
   {
      output = ObjectRef(new GrowingFrameBuffer<float> (outputLength, outputLookAhead+outputLookBack+1));
   } else {
      output = ObjectRef(new RotatingFrameBuffer<float> (outputLength, outputLookAhead+outputLookBack+1));
   }
}

void BufferedNode::request(const ParameterSet &req)
{
   if (req.exist("LOOKAHEAD"))
   {
      outputLookAhead = max(outputLookAhead,dereference_cast<int> (req.get("LOOKAHEAD")));
   }
   if (req.exist("LOOKBACK"))
      outputLookBack = max(outputLookBack,dereference_cast<int> (req.get("LOOKBACK")));
   this->Node::request(req);
}
