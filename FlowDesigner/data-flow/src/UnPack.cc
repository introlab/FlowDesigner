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
 * @output_name NOT_END
 * @output_description True if there's still data
 *
END*/


UnPack::UnPack(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   outputID = addOutput("OUTPUT");
   endID = addOutput("NOT_END");
   inputID = addInput("INPUT");
}

void UnPack::initialize()
{
   processCount = -1;
   this->Node::initialize();
}

void UnPack::reset()
{
   processCount = -1;
   this->Node::reset();
}

ObjectRef UnPack::getOutput(int output_id, int count)
{
   //cerr << "Getting output in UnPack\n";
   if (output_id==outputID || output_id==endID)
   {
      processCount=count;
      
      NodeInput input = inputs[inputID];
      ObjectRef inputValue = input.node->getOutput(input.outputID,0);
      
      Vector<ObjectRef> &packed = object_cast <Vector<ObjectRef> > (inputValue);
      if (count < int(packed.size()))
      {
         if (output_id==outputID)
	    return packed[count];
	 else
	    return TrueObject;
      }
      else
      {
         if (output_id==outputID)
	    return nilObject;
	 else
	    return FalseObject;
      }
   }
   else 
      throw new NodeException (this, "UnPack: Unknown output id", __FILE__, __LINE__);
}
