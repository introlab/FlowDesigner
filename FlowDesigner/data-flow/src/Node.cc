// Copyright (C) 1998-1999 Jean-Marc Valin & Dominic Letourneau
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

Node::Node(string nodeName, ParameterSet params) 
   : name (nodeName)
   , inputs (vector<NodeInput>(0))
   , initialized (false)
   , debugMode (false)
   , processCount(-1)
   , outputInitializeCount (0)
   , parameters(params)
{}

void Node::connectToNode(unsigned int in, Node *inputNode, unsigned int out)
{
   if (in >= inputs.size()) inputs.resize(in);
   inputs[in] = NodeInput(inputNode, out);
   inputNode->registerOutput(out);
}

void Node::connectToNode(string in, Node *inputNode, string out)
{
   connectToNode(translateInput(in), inputNode, inputNode->translateOutput(out));
}

void Node::initialize ()
{
   if (initialized) return;
   if (--outputInitializeCount <=0)
   {
      this->specificInitialize();
      vector<NodeInput>::iterator in;
      for (in = inputs.begin(); in < inputs.end(); in++)
      {
         in->node->initialize();
         if (!in->node->hasOutput(in->outputID)) throw "no output";
      }
   }
}

void Node::specificInitialize()
{
   initialized = true;
}

void Node::reset()
{
   if (initialized)
   {
      vector<NodeInput>::iterator in;
      for (in = inputs.begin(); in < inputs.end(); in++)
      {
         in->node->incrementOutputInitialize();
      }  
   }
   processCount=-1;
}
