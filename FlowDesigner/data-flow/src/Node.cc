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
#include <string>
#include <typeinfo>
#include "Object.h"

void ParameterSet::requireParam(string param) 
{
   if (find(param)==end()) 
      throw new MissingParameterException(param,*this);
}

ObjectRef ParameterSet::get(string param) 
{
   if (find(param)==end())
      throw new MissingParameterException(param,*this);
   else return operator[](param);
}
ObjectRef ParameterSet::getDefault(string param, ObjectRef value) 
{
   if (find(param)==end()) 
      return value;
   else return operator[](param);
}
void ParameterSet::defaultParam(string param, ObjectRef value)
{
   if (find(param)==end())
      operator[](param)=value;
}
void ParameterSet::add(string param, ObjectRef value)
{
   operator[](param)=value;
}

void ParameterSet::print (ostream &out)
{
   for (ParameterSet::iterator it=begin(); it!=end();it++)
      out << it->first << " -> " << typeid(*(it->second)).name() << endl;
}

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
   if (in >= inputs.size()) inputs.resize(in + 1);
   cout<<"Adding input: "<<inputNode->name<<" to: "<<name<<endl;
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
      specificInitialize();
      cout<<name<<" is in initialize..."<<endl;


      vector<NodeInput>::iterator in;

      for (in = inputs.begin(); in < inputs.end(); in++)
      {
         cout<<in->node->name<<"called for initialized..."<<endl;
         in->node->initialize();

         if (!in->node->hasOutput(in->outputID)) 
            throw new NodeException(this, "Input node doesn't implement output", __FILE__, __LINE__);

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
