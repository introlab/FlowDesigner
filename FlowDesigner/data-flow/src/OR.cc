// Copyright (C) 1999-2001 Dominic Letourneau & Jean-Marc Valin

#include "BufferedNode.h"
#include "Exception.h"

class OR;

DECLARE_NODE(OR)
/*Node
 *
 * @name OR
 * @category Logic
 * @description Logical OR between two inputs
 *
 * @input_name INPUT1
 * @input_type bool
 * @input_description First boolean input
 *
 * @input_name INPUT2
 * @input_type bool
 * @input_description Second boolean input
 *
 * @output_name OUTPUT
 * @output_type bool
 * @output_description Boolean output
 *
 * @parameter_name PULL_ANYWAY
 * @parameter_type bool
 * @parameter_description Pull on INPUT2 even if INPUT1 is true
 * @parameter_value false
 *
END*/

class OR : public BufferedNode {
   int input1ID;
   int input2ID;
   int outputID;
   bool pullAnyway;
public:


   OR(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      input1ID = addInput("INPUT1");
      input2ID = addInput("INPUT2");
      outputID = addOutput("OUTPUT");
      if (parameters.exist("PULL_ANYWAY"))
	 pullAnyway = dereference_cast<bool> (parameters.get("PULL_ANYWAY"));
      else
	 pullAnyway = false;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      if (pullAnyway)
      {

	 ObjectRef input1Value = getInput(input1ID, count);
	 ObjectRef input2Value = getInput(input2ID, count);
	 if (dereference_cast<bool> (input1Value)
	     || dereference_cast<bool> (input2Value))
	    out[count] = TrueObject;
	 else
	    out[count] = FalseObject;

      } else {

	 ObjectRef input1Value = getInput(input1ID, count);
	 if (dereference_cast<bool> (input1Value))
	 {
	    out[count] = TrueObject;
	    return;
	 }
	 ObjectRef input2Value = getInput(input2ID, count);
	 if (dereference_cast<bool> (input2Value))
	 {
	    out[count] = TrueObject;
	    return;
	 }
	 out[count] = FalseObject;

      }
   }
};
