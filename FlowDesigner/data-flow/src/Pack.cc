// Copyright (C) 1999 Jean-Marc Valin

#include "Pack.h"
#include "net_types.h"
#include "Vector.h"

DECLARE_NODE(Pack)
/*Node
 *
 * @name Pack
 * @category Flow
 * @description Pack Data into a vector
 *
 * @input_name INPUT
 * @input_description Inputs to be packed (until processCount reached)
 *
 * @output_name OUTPUT
 * @output_description A vector of ObjectRef(s)
 * @output_type Vector<ObjectRef>
 *
END*/


Pack::Pack(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   outputID = addOutput("OUTPUT");
   inputID = addInput("INPUT");
}

void Pack::specificInitialize()
{
   processCount=-1;
   this->Node::specificInitialize();
   output = ObjectRef (new Vector<ObjectRef>);
}

void Pack::reset()
{
   processCount = -1;
   this->Node::reset();
   output = ObjectRef (new Vector<ObjectRef>);
}

ObjectRef Pack::getOutput(int output_id, int count)
{
   //cerr << "Getting output in Pack\n";
   if (output_id==outputID)
   {
      while (processCount < count)
      {
         processCount++;
         Vector<ObjectRef> &pack = object_cast<Vector<ObjectRef> > (output);
         
         NodeInput input = inputs[inputID];
         ObjectRef inputValue = input.node->getOutput(input.outputID,processCount);
         
         pack.insert(pack.end(), inputValue);
      }
      //cerr << "Pack returning: " << output << " (" << typeid(output).name() << ")" << endl;
      return output;
   }
   else 
      throw new NodeException (this, "Pack: Unknown output id", __FILE__, __LINE__);
}
