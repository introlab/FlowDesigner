#include "OutputStream.h"
#include "net_types.h"

OutputStream::OutputStream(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   outputID = addOutput("OUTPUT");
   inputID = addInput("INPUT");
}

void OutputStream::specificInitialize()
{
   this->Node::specificInitialize();
}

void OutputStream::reset()
{
   this->Node::reset();
}

ObjectRef OutputStream::getOutput(int output_id, int count)
{
   if (output_id==outputID)
   {
      if (count != processCount)
      {
         NodeInput input = inputs[inputID];
         string fileName = dereference_cast<string> (input.node->getOutput(input.outputID,count));
         openedFile = ObjectRef (new OFStream());
         ofstream &tmp = dereference_cast<ofstream> (openedFile);
         tmp.open(fileName.c_str());
      }
      return openedFile;
   }
   else 
      throw NodeException (this, "OutputStream: Unknown output id", __FILE__, __LINE__);
}
