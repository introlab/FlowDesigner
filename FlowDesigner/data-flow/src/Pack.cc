// Copyright (C) 1999 Jean-Marc Valin

#include "Pack.h"
#include "net_types.h"
#include "Vector.h"

using namespace std;
using namespace FD;

DECLARE_NODE(Pack)
/*Node
 *
 * @name Pack
 * @category Flow
 * @description Pack Data into a vector of Objects references. When the node is in the main network or in a sub-network, his input is packed in the vector only once. However while in iterators, his input is packed (added) in the vector at every iteration.
 *
 * @input_name INPUT
 * @input_description Objects to be packed (until processCount reached)
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

void Pack::initialize()
{
   processCount=-1;
   this->Node::initialize();
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
