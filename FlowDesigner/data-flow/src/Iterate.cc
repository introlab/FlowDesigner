// Copyright (C) 1999 Jean-Marc Valin

#include "Node.h"

using namespace std;
using namespace FD;

class Iterate;

DECLARE_NODE(Iterate)
/*Node

 * @name Iterate
 * @category Flow
 * @description Specify the number of iteration to do (max). Therefore, it can only be used in iterators and the output must be set to COND (left click on the node output and hold the control).

 * @output_name OUTPUT
 * @output_description Return true if count < MAX_ITER
 * @output_type bool

 * @parameter_name MAX_ITER
 * @parameter_description Number of iteration to do (max)
 * @parameter_type int

END*/


class Iterate : public Node {
protected:
   ///The ID of the 'output' output
   int outputID;

   int maxIter;
   ObjectRef trueObject;
   ObjectRef falseObject;
public:
   ///Constructor, takes the name of the node and a set of parameters
   Iterate(string nodeName, ParameterSet params) : Node (nodeName,params)
   {
      outputID = addOutput ("OUTPUT");
      trueObject = ObjectRef (new Bool(true));
      falseObject = ObjectRef (new Bool(false));
      maxIter = dereference_cast<int> (parameters.get("MAX_ITER"));
   }
   
   virtual ObjectRef getOutput(int output_id, int count) 
   {
      if (count < maxIter)
	 return trueObject;
      else
	 return falseObject;
   }
   
};
