// Copyright (C) 1999 Jean-Marc Valin

#include "Node.h"
#include "Vector.h"

class Accumulate;

DECLARE_NODE(Accumulate)
/*Node
 *
 * @name Accumulate
 * @category General
 * @description Accumulation of frames into a buffer
 *
 * @input_name INPUT
 * @input_description Input object
 * @input_type Vector<float>
 *
 * @input_name ACCUM
 * @input_description Accumulator where to put the input
 * @input_type Vector<ObjectRef>
 *
 * @output_name OUTPUT
 * @output_description The input accumulator
 * @output_type Vector<ObjectRef>
 *
END*/

class Accumulate : public Node {
protected:
   int inputID;
   int accumID;
   int outputID;
   //int processCount;
public:
   Accumulate (string nodeName, const ParameterSet &params)
      : Node(nodeName, params)
   {
      inputID = addInput("INPUT");
      accumID = addInput("ACCUM");
      outputID = addOutput("OUTPUT");
   }

   void specificInitialize()
   {
      //processCount = -1;
      Node::specificInitialize();
   }

   /**Propagate requests*/
   virtual void request(int outputID, const ParameterSet &req)
   {
      inputs[inputID].node->request(inputs[inputID].outputID, req);
      inputs[accumID].node->request(inputs[accumID].outputID, req);
   }

   void reset()
   {
      //processCount = -1;
      Node::reset();
   }

   ObjectRef getOutput(int output_id, int count)
   {
      //cerr << "Accum" << endl;
      //int i,j;
      //for (i=processCount+1;i<=count;i++)
      //{
	 ObjectRef inputValue = getInput(inputID,count);
	 ObjectRef accumValue = getInput(accumID,count);
	 Vector<ObjectRef> &accum = object_cast<Vector<ObjectRef> > (accumValue);
	    
	 accum.push_back(inputValue);
	 
      //}
      //processCount = count;
      return getInput(accumID,count);
   }


};
