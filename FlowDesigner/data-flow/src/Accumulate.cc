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
 * @input_name ACCUM
 * @input_description Accumulator where to put the input
 * @input_type Vector<ObjectRef>
 *
 * @input_name INPUT
 * @input_description Input object
 * @input_type any
 *
 * @output_name OUTPUT
 * @output_description The input accumulator
 * @output_type Vector<ObjectRef>
 *
END*/

class Accumulate : public Node {
protected:
   int accumID;
   int outputID;
public:
   Accumulate (string nodeName, const ParameterSet &params)
      : Node(nodeName, params)
   {
      accumID = addInput("ACCUM");
      outputID = addOutput("OUTPUT");
   }

   virtual int translateInput (string inputName)
   {
      for (unsigned int i=0; i< inputs.size(); i++)
      {
         if (inputs[i].name == inputName)
         {
            return i;
         }
      }
      return addInput(inputName);
   }

   ObjectRef getOutput(int output_id, int count)
   {
      ObjectRef accumValue = getInput(accumID,count);
      Vector<ObjectRef> &accum = object_cast<Vector<ObjectRef> > (accumValue);

      for (int j = 1; j < inputs.size(); j++)
      {
         ObjectRef inputValue = getInput(j,count);
         accum.push_back(inputValue);
      }
      return accumValue;
   }


};
