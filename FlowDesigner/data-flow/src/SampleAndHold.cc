// Copyright (C) 1999 Jean-Marc Valin

#include "Node.h"
#include "ObjectRef.h"
#include <math.h>

class SampleAndHold;
DECLARE_NODE(SampleAndHold)
/*Node
 *
 * @name SampleAndHold
 * @category Flow
 * @description Downsamples in the "count" domain
 *
 * @input_name INPUT
 * @input_description The input x[count]
 *
 * @output_name OUTPUT
 * @output_description x[count - count%FACTOR]
 *
 * @parameter_name DOWNSAMPLING
 * @parameter_description The downsampling factor
 * @parameter_type int
 *
END*/


class SampleAndHold : public Node {
protected:
   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'input' input*/
   int inputID;

   /**input/output count ratio*/
   int down;
public:
   ///Constructor, takes the name of the node and a set of parameters
   SampleAndHold(string nodeName, ParameterSet params) : Node (nodeName,params)
   {
      inputID = addInput ("INPUT");
      outputID = addOutput ("OUTPUT");
      down = dereference_cast<int> (parameters.get("DOWNSAMPLING"));
   }
   
   virtual void specificInitialize()
   {
      this->Node::specificInitialize();
      ParameterSet req;
      req.add("LOOKBACK", ObjectRef(new Int(down)));
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
	 p.add("LOOKAHEAD", ObjectRef(Int::alloc(dereference_cast<int> (req.get("LOOKAHEAD")))));
	 inputs[inputID].node->request(inputs[inputID].outputID,p);
      }
      if (req.exist("LOOKBACK"))
      {
	 int look = dereference_cast<int> (req.get("LOOKBACK")) + down;

	 ParameterSet p;
	 p.add("LOOKBACK", ObjectRef(new Int (look)));
	 inputs[inputID].node->request(inputs[inputID].outputID,p);
      }
   
      //if (req.exist("CACHEALL"))
      
      this->Node::request(outputID,req);
      
   }
      

};

ObjectRef SampleAndHold::getOutput(int output_id, int count)
{
   return getInput(output_id, count-count%down);
}
