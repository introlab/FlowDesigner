// Copyright (C) 1999-2001 Dominic Letourneau & Jean-Marc Valin

#include "BufferedNode.h"
#include "Exception.h"

using namespace std;
using namespace FD;

class NOT;

DECLARE_NODE(NOT)
/*Node
 *
 * @name NOT
 * @category Logic
 * @description Logical NOT of an input
 *
 * @input_name INPUT
 * @input_type bool
 * @input_description Boolean input
 *
 * @output_name OUTPUT
 * @output_type bool
 * @output_description Boolean output
 *
END*/

class NOT : public BufferedNode {
   int inputID;
   int outputID;
public:


   NOT(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);
      if (dereference_cast<bool> (inputValue))
	 out[count] = FalseObject;
      else
	 out[count] = TrueObject;

   }
};
