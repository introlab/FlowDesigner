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

#ifndef Collector_h
#define Collector_h

#include "Node.h"
#include "Exception.h"
#include <map>

class Collector : public Node
{
 
public:

   ///Constructor, takes the name of the node and a set of parameters
   Collector(string nodeName, ParameterSet params);

   /**Class specific initialization routine.
      Each class will call its subclass specificInitialize() method*/
   virtual void specificInitialize();

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count); 
  
   /**Ask for the node's output (named) and for the count iteration */
   virtual ObjectRef getOutputNamed (const string &outputName, int count);
   

   ///Checks whether node really has a certain output
   virtual bool hasOutput(int output_id) const;

protected:
   ///symbolic to numeric translation for input names
   virtual int translateInput(string inputName);

   ///symbolic to numeric translation for output names
   virtual int translateOutput(string inputName);

protected:
   ///Default constructor, should not be used
   Collector() {throw(GeneralException("Collector default constructor should not be called",__FILE__,__LINE__));}

};

#endif
