// Copyright (C) 1999 Jean-Marc Valin

#include "Node.h"
#include "Vector.h"

class Accumulate;

DECLARE_NODE(Accumulate)
/*Node
 *
 * @name Accumulate
 * @category General
 * @description Accumulation of objects into a buffer, that is, a vector of Objects References. When the node is in the main network or in a sub-network, his input is packed in the vector only once. However while in iterators, his input is packed (added) in the vector at every iteration. As well, his other input "ACCUM" must be connected to a node: "NewAccumulator"(General).
 *
 * @input_name INPUT
 * @input_description Input object
 * @input_type any
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

   void initialize()
   {
      //processCount = -1;
      Node::initialize();
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
      return accumValue;
   }


};
