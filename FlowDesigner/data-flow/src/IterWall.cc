// Copyright (C) 1999 Jean-Marc Valin

#include "Node.h"

class IterWall;

DECLARE_NODE(IterWall)
/*Node
 *
 * @name IterWall
 * @category Flow
 * @description Get the input object only once
 *
 * @input_name INPUT
 * @input_description The input object
 *
 * @output_name OUTPUT
 * @output_description The output object = the input object (calculated once)
 *
END*/


class IterWall : public Node {
protected:
   int inputID;
   int outputID;
   int iter;
      ObjectRef value;
      bool calculated;

public:
   IterWall(string nodeName, ParameterSet params)
      : Node(nodeName, params)
   {
      try {
         inputID = addInput("INPUT");
	 outputID=addOutput("OUTPUT");
	 if (parameters.exist("ITER"))
	    iter = dereference_cast<int> (parameters.get("ITER"));
	 else
	    iter = 0;
      } catch (BaseException *e)
      {
         throw e->add(new NodeException (NULL, "Exception caught in IterWall constructor", 
					 __FILE__, __LINE__));
      }
      
   }

   virtual void specificInitialize()
   {
      Node::specificInitialize();
      calculated = false;
   }

   virtual void reset()
   {
      Node::reset();
      calculated = false;
   }

   ObjectRef getOutput(int output_id, int count)
   {
      if (!calculated)
      {
	 value = getInput(inputID, iter);
      }
      return value;
   }

};
