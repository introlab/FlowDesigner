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


#include "Node.h"
#include "Vector.h"
#include "ObjectParser.h"

#include "variables.h"

class VarLoad;

DECLARE_NODE(VarLoad)
/*Node

 * @name VarLoad
 * @category General
 * @description Load a variable

 * @output_name OUTPUT
 * @output_description The variable value

 * @parameter_name VARIABLE
 * @parameter_description The name of the variable
 * @parameter_type string

END*/


/** A constant node contains a value that will never changes. */
class VarLoad : public Node
{

protected:

   /**The ID of the 'value' output*/
   int outputID;

   String varName;

public:

   /**Constructor, takes the name of the node and a set of parameters*/
   VarLoad(string nodeName, ParameterSet params)
      : Node(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      
      varName = object_cast<String> (parameters.get("VARIABLE"));
   }

   void specificInitialize()
   {
      Node::specificInitialize();
   }
      
   virtual ~VarLoad()
   {
   }

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count)
   {
      if (output_id==outputID) 
      {

	 map<string,ObjectRef>::iterator tmp = Variable::all.find(varName);
	 if (tmp!=Variable::all.end())
	    return tmp->second;
	 else 
	    return Object::nilObject;

	 /*openedFile = ObjectRef (new IFStream());
	 IFStream &tmp = object_cast<IFStream> (openedFile);
	 tmp.open(fileName.c_str());*/
      }
      else throw new NodeException (this, "VarLoad: Unknown output id", __FILE__, __LINE__);
      
      
   }

protected:
   /**Default constructor, should not be used*/
   VarLoad() {throw new GeneralException("VarLoad copy constructor should not be called",__FILE__,__LINE__);}

};
