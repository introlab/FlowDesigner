#include "InputStream.h"
#include "net_types.h"

InputStream::InputStream(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   outputID = addOutput("OUTPUT");
   inputID = addInput("INPUT");
}

void InputStream::specificInitialize()
{
   this->Node::specificInitialize();
}

ObjectRef InputStream::getOutput(int output_id, int count)
{
   if (output_id==outputID)
   {
      if (count != processCount)
      {
         NodeInput input = inputs[inputID];
         string fileName = dereference_cast<string> (input.node->getOutput(input.outputID,count));
         openedFile = ObjectRef (new IFStream());
         ifstream &tmp = dereference_cast<ifstream> (openedFile);
         tmp.open(fileName.c_str());
      }
      return openedFile;
   }
   else 
      throw NodeException (this, "InputStream: Unknown output id", __FILE__, __LINE__);
}
