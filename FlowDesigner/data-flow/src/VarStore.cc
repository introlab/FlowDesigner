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

class VarStore;

DECLARE_NODE(VarStore)
/*Node

 * @name VarStore
 * @category General
 * @description No description available

 * @input_name INPUT
 * @input_description No description available

 * @output_name OUTPUT
 * @output_description No description available

 * @parameter_name VARIABLE
 * @parameter_description No description available

END*/


/** A constant node contains a value that will never changes. */
class VarStore : public Node
{

protected:

   /**The ID of the 'OUTPUT' output*/
   int outputID;

   /**The ID of the 'INPUT' input*/
   int inputID;

   String varName;

public:

   /**Constructor, takes the name of the node and a set of parameters*/
   VarStore(string nodeName, ParameterSet params)
      : Node(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      inputID = addInput("INPUT");

      varName = object_cast<String> (parameters.get("VARIABLE"));
   }

   void specificInitialize()
   {
      Node::specificInitialize();
   }
      
   virtual ~VarStore()
   {
   }

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count)
   {
      if (output_id==outputID) 
      {
	 NodeInput input = inputs[inputID];
	 ObjectRef inputValue = input.node->getOutput(input.outputID,count);

	 Variable::all[varName] = inputValue;
	 return inputValue;

	 /*
	 map<string,ObjectRef>::iterator tmp = Variable::all.find(varName);
	 if (tmp!=Variable::all.end())
	    return tmp->second;
	 else 
	    return Object::nilObject;
	 */

	 /*openedFile = ObjectRef (new IFStream());
	 IFStream &tmp = object_cast<IFStream> (openedFile);
	 tmp.open(fileName.c_str());*/
      }
      else throw new NodeException (this, "VarStore: Unknown output id", __FILE__, __LINE__);
      
      
   }

protected:
   /**Default constructor, should not be used*/
   VarStore() {throw new GeneralException("VarStore copy constructor should not be called",__FILE__,__LINE__);}

};
