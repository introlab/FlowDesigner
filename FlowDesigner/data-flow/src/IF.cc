// Copyright (C) 1999 Jean-Marc Valin

#include "Node.h"

using namespace std;

class IF;

DECLARE_NODE(IF)
/*Node
 *
 * @name IF
 * @category Logic
 * @description Takes a branch or another depending on a condition (Bool value).
 *
 * @input_name COND
 * @input_description The condition for the if statement
 * @input_type bool
 *
 * @input_name THEN
 * @input_description What to do if the condition is true
 *
 * @input_name ELSE
 * @input_description What to do if the condition is false
 *
 * @output_name OUTPUT
 * @output_description The object from THEN or ELSE depending on COND
 *
 * @parameter_name PULL_ANYWAY
 * @parameter_type bool
 * @parameter_description If true, the IF statement pulls also on the branch not taken
 *
END*/


class IF : public Node {
protected:
   int inputID;
   int thenID;
   int elseID;
   int outputID;
   bool pullAnyway;

public:
   IF(string nodeName, ParameterSet params)
      : Node(nodeName, params)
   {
      try
      {
         inputID = addInput("COND");
         thenID = addInput("THEN");
         elseID = addInput("ELSE");
	 outputID=addOutput("OUTPUT");
	 if (parameters.exist("PULL_ANYWAY"))
	 {
	    pullAnyway = dereference_cast<bool> (parameters.get("PULL_ANYWAY"));
	 }
	 else
	 {
	    pullAnyway = false;
	 }
      }
      catch (BaseException *e)
      {
         throw e->add(new NodeException (NULL, "Exception caught in IF constructor", __FILE__, __LINE__));
      }
      
   }

   ObjectRef getOutput(int output_id, int count)
   {
      //NodeInput input = inputs[inputID];
      //ObjectRef inputValue = input.node->getOutput(input.outputID,count);
      if(dereference_cast<bool> (getInput(inputID,count)))
      {
	 if(pullAnyway)
	 {
	    getInput(elseID,count);
	 }
	 return getInput(thenID,count);
      } 
      else
      {
	 if(pullAnyway)
	 {
	    getInput(thenID,count);
	 }
	 return getInput(elseID,count);
      }
   }

};
