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

/*
** Includes
*/
#include "Collector.h"
//DECLARE_NODE(Collector)
NODE_INFO(Collector,"Flow", "INPUT", "OUTPUT", "")

Collector::Collector(string nodeName, ParameterSet params) 
   : Node(nodeName, params) 
{
}

ObjectRef Collector::getOutputNamed (const string &outputName, int count) {
   
   for (int i=0; i< inputs.size(); i++) {
      if (inputs[i].name == outputName) {
         return getOutput(i,count);
      }
   }   
 
   throw NodeException(this,string("Unknown output name :")+outputName,__FILE__,__LINE__);

   //just in case returning a nil object
   return ObjectRef(new Object(Object::nil));
}


void Collector::specificInitialize()
{
   this->Node::specificInitialize();
}

int Collector::translateInput (string inputName)
{

   for (int i=0; i< inputs.size(); i++) {
      if (inputs[i].name == inputName) {
         return i;
      }
   }    

   return addInput(inputName);
}

int Collector::translateOutput (string outputName)
{
  // Simply call translateInput because it should return
  // the same integer...
  return translateInput(outputName);
}

bool Collector::hasOutput(int output_id) const
{
  return(inputs.size() > output_id);
}

ObjectRef Collector::getOutput(int output_id, int count)
{
  
   if (hasOutput(output_id)) {
      int outputID = inputs[output_id].outputID;
      return (inputs[output_id].node)->getOutput(outputID,count);
   }
   else {
      throw NodeException(this,"Unknown output_id",__FILE__,__LINE__);
   }
   
   //Just in case returns an invalid object
   return ObjectRef(new Object(Object::nil));
}




