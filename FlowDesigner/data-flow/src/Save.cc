#include "Save.h"
#include "net_types.h"
#include "Object.h"

Save::Save(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   outputID = addOutput("OUTPUT");
   streamInputID = addInput("STREAM");
   objectInputID = addInput("OBJECT");
}

void Save::specificInitialize()
{
   this->Node::specificInitialize();
}

ObjectRef Save::getOutput(int output_id, int count)
{
   if (output_id==outputID)
   {
      if (count != processCount)
      {
         NodeInput streamInput = inputs[streamInputID];
         ofstream &stream = dereference_cast<ofstream> (streamInput.node->getOutput(streamInput.outputID,count));
         NodeInput objectInput = inputs[objectInputID];
         Object &object = *(objectInput.node->getOutput(objectInput.outputID,count));
         stream << object;
         stream.flush();
      }
      return ObjectRef();
   }
   else 
      throw NodeException (this, "Save: Unknown output id", __FILE__, __LINE__);
}
