// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau


#include "Node.h"
#include "ObjectParser.h"

#include "variables.h"

using namespace std;

namespace FD {

class VarLoad;

DECLARE_NODE(VarLoad)
/*Node
 *
 * @name VarLoad
 * @category General
 * @description Load a variable. The variable is pulled from a node of type: " VarStore "(General) which has the same given name. The node: " VarStore " can be declared in a different Overflow file, but must be declared before.
 *
 * @output_name OUTPUT
 * @output_description The variable value
 *
 * @parameter_name VARIABLE
 * @parameter_description The name of the variable t be loaded
 * @parameter_type string
 *
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

   virtual ObjectRef getOutput(int output_id, int count)
   {
      if (output_id==outputID) 
      {

	 map<string,ObjectRef>::iterator tmp = Variable::all.find(varName);
	 if (tmp!=Variable::all.end())
	    return tmp->second;
	 else 
	    throw new NodeException (this, string("VarLoad: Unknown variable: ") + varName, __FILE__, __LINE__);
      }
      else throw new NodeException (this, "VarLoad: Unknown output id", __FILE__, __LINE__);
      
      
   }

};

}//namespace FD
