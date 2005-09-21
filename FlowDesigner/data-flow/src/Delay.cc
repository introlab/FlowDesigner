// Copyright (C) 1999 Jean-Marc Valin

#include "Node.h"
#include "ObjectRef.h"
#include <math.h>

using namespace std;

namespace FD {

class Delay;
DECLARE_NODE(Delay)
/*Node
 *
 * @name Delay
 * @category Flow
 * @description Delay the input of DELAY iterations. Therefore, it can only be used in iterators.
 *
 * @input_name INPUT
 * @input_description The input object
 *
 * @output_name OUTPUT
 * @output_description The output object = input object with a delay
 *
 * @parameter_name DELAY
 * @parameter_description The delay
 * @parameter_type int
 *
END*/


class Delay : public Node {
protected:
   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'input' input*/
   int inputID;

   /**input/output count ratio*/
   int delay;
public:
   ///Constructor, takes the name of the node and a set of parameters
   Delay(string nodeName, ParameterSet params) : Node (nodeName,params)
   {
      inputID = addInput ("INPUT");
      outputID = addOutput ("OUTPUT");
      delay = dereference_cast<int> (parameters.get("DELAY"));
   }
   
   virtual ObjectRef getOutput(int output_id, int count);
   
   void request(int outputID, const ParameterSet &req)
   {
      //cerr << "name = " << name << " this = " << this << " outputID = " << outputID << endl;   cerr << "lookahead = " << outputs[outputID].lookAhead << " lookback = " << outputs[outputID].lookBack << endl;   
      
      if (req.exist("LOOKAHEAD"))
      {
	 int look = dereference_cast<int> (req.get("LOOKAHEAD")) - delay;

	 if (look > 0)
	 {
	    ParameterSet p;
	    p.add("LOOKAHEAD", ObjectRef(Int::alloc(look)));
	    inputs[inputID].node->request(inputs[inputID].outputID,p);
	 }
      }
      if (req.exist("LOOKBACK"))
      {
	 int look = dereference_cast<int> (req.get("LOOKBACK")) + delay;

	 if (look > 0)
	 {

	    ParameterSet p;
	    p.add("LOOKBACK", ObjectRef(Int::alloc(look)));
	    inputs[inputID].node->request(inputs[inputID].outputID,p);
	 }
      }
   }
      

};

ObjectRef Delay::getOutput(int output_id, int count)
{
   NodeInput input = inputs[inputID];
   if (count-delay >= 0)
      return input.node->getOutput(input.outputID,count-delay);
   else 
      return nilObject;
}

}//namespace FD
