// Copyright (C) 1999 Jean-Marc Valin

#include "Node.h"
#include "ObjectRef.h"
#include <math.h>

class Sync;
DECLARE_NODE(Sync)
/*Node
 *
 * @name Sync
 * @category Flow
 * @description No-op node for which count ratio (getInput/getOutput) = RATIO
 *
 * @input_name INPUT
 * @input_description Input
 *
 * @output_name OUTPUT
 * @output_description Output (no-op) same as input with different count
 *
 * @parameter_name RATIO
 * @parameter_description (input/output) count ratio
 *
END*/


class Sync : public Node {
protected:
   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'input' input*/
   int inputID;

   /**input/output count ratio*/
   float ratio;
public:
   ///Constructor, takes the name of the node and a set of parameters
   Sync(string nodeName, ParameterSet params) : Node (nodeName,params)
   {
      inputID = addInput ("INPUT");
      outputID = addOutput ("OUTPUT");
      ratio = dereference_cast<float> (parameters.get("RATIO"));
   }
   
   virtual void specificInitialize()
   {
      this->Node::specificInitialize();
      
      ParameterSet req;
      req.add("LOOKAHEAD", ObjectRef(Int::alloc(int(floor (ratio)))));
      inputs[inputID].node->request(inputs[inputID].outputID, req);

   }

   virtual void reset()
   {
      this->Node::reset();
   }

   virtual ObjectRef getOutput(int output_id, int count);
   

   void request(int outputID, const ParameterSet &req)
   {
      //cerr << "name = " << name << " this = " << this << " outputID = " << outputID << endl;   cerr << "lookahead = " << outputs[outputID].lookAhead << " lookback = " << outputs[outputID].lookBack << endl;   
      
      if (req.exist("LOOKAHEAD"))
      {
	 ParameterSet p;
	 p.add("LOOKAHEAD", ObjectRef(Int::alloc(int(1+ratio*dereference_cast<int> (req.get("LOOKAHEAD"))))));
	 inputs[inputID].node->request(inputs[inputID].outputID,p);
      }
      if (req.exist("LOOKBACK"))
      {
	 ParameterSet p;
	 p.add("LOOKBACK", ObjectRef(Int::alloc(int(1+ratio*dereference_cast<int> (req.get("LOOKBACK"))))));
	 inputs[inputID].node->request(inputs[inputID].outputID,p);
      }
      //if (req.exist("CACHEALL"))
	 
      this->Node::request(outputID,req);
      
   }
      

};

ObjectRef Sync::getOutput(int output_id, int count)
{
   NodeInput input = inputs[inputID];
   return input.node->getOutput(input.outputID,int(floor(.5+count*ratio)));
}
