#include "Sum.h"
#include "net_types.h"
#include "Object.h"

Sum::Sum(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   outputID = addOutput("OUTPUT");
   inputID = addInput("INPUT");
}

void Sum::specificInitialize()
{
   this->Node::specificInitialize();
   sum = 0;
}

void Sum::reset()
{
   this->Node::reset();
}

ObjectRef Sum::getOutput(int output_id, int count)
{
   if (output_id==outputID)
   {
      for (int i=processCount+1 ; i<=count ; i++ )
      {
         NodeInput input = inputs[inputID];
         ObjectRef inputResult = input.node->getOutput(input.outputID, i);
         if (!inputResult->status)
         {
            sum += dereference_cast<float> (inputResult);
            cerr << "sum = " << sum << endl;
         }
         processCount = i;
      }
      return ObjectRef(new Float(sum));
   }
   else 
      throw NodeException (this, "Sum: Unknown output id", __FILE__, __LINE__);
}
