// Copyright (C) 1999 Jean-Marc Valin

#include "Node.h"

class IterCount;
DECLARE_NODE(IterCount)
/*Node

 * @name IterCount
 * @category Logic
 * @description Get the iterator count (iteration number)

 * @output_name OUTPUT
 * @output_description The iteration count
 * @output_type int

END*/


class IterCount : public Node {
protected:
   ///The ID of the 'output' output
   int outputID;

public:
   ///Constructor, takes the name of the node and a set of parameters
   IterCount(string nodeName, ParameterSet params) : Node (nodeName,params)
   {
      outputID = addOutput ("OUTPUT");
   }
   
   virtual void specificInitialize()
   {
      this->Node::specificInitialize();
   }

   virtual ObjectRef getOutput(int output_id, int count) 
   {
      return ObjectRef(new Int(count));
   }
   
};
