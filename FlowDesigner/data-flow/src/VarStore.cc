// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau


#include "Node.h"
#include "Vector.h"
#include "ObjectParser.h"

#include "variables.h"

class VarStore;

DECLARE_NODE(VarStore)
/*Node

 * @name VarStore
 * @category General
 * @description Store a variable (named)

 * @input_name INPUT
 * @input_description The value of the variable 

 * @output_name OUTPUT
 * @output_description The value of the variable

 * @parameter_name VARIABLE
 * @parameter_description The variable name
 * @parameter_type string

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
