// Copyright (C) 1999 Jean-Marc Valin

#include "UnPack.h"
#include "net_types.h"
#include "Vector.h"

DECLARE_NODE(UnPack)
/*Node
 *
 * @name UnPack
 * @category Flow
 * @description Unpack data already packed
 *
 * @input_name INPUT
 * @input_description The packed vector
 *
 * @output_name OUTPUT
 * @output_description The single unpacked Object
 *
END*/


UnPack::UnPack(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   outputID = addOutput("OUTPUT");
   inputID = addInput("INPUT");
}

void UnPack::specificInitialize()
{
   processCount = -1;
   this->Node::specificInitialize();
}

void UnPack::reset()
{
   processCount = -1;
   this->Node::reset();
}

ObjectRef UnPack::getOutput(int output_id, int count)
{
   //cerr << "Getting output in UnPack\n";
   if (output_id==outputID)
   {
      processCount=count;
      
      NodeInput input = inputs[inputID];
      ObjectRef inputValue = input.node->getOutput(input.outputID,0);
      
      Vector<ObjectRef> &packed = object_cast <Vector<ObjectRef> > (inputValue);
      if (count < packed.size())
      {
         //cerr << packed[count] << endl;
         return packed[count];
      }
      else
      {
         //cerr << "Past the end in Unpack\n";
         return Object::past_endObject;
      }
   }
   else 
      throw new NodeException (this, "UnPack: Unknown output id", __FILE__, __LINE__);
}
