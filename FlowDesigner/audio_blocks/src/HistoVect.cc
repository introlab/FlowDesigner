// Copyright (C) 1999 Jean-Marc Valin

#include "FrameOperation.h"
#include "Buffer.h"
#include "Vector.h"

class HistoVect;

DECLARE_NODE(HistoVect)
/*Node

 * @name HistoVect
 * @category Signal:Manip
 * @description No description available

 * @input_name INPUT
 * @input_description No description available

 * @output_name OUTPUT
 * @output_description No description available

 * @parameter_name LENGTH
 * @parameter_description No description available

END*/


class HistoVect : public FrameOperation {
   
   int inputID;

public:
   HistoVect(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      inputID = addInput("INPUT");
   }

   ~HistoVect() {}

   virtual void specificInitialize()
   {
      this->FrameOperation::specificInitialize();
   }

      //virtual void request(int outputID, const ParameterSet &req) {inputs[inputID].node->request(outputID,req);}
      
   void calculate(int output_id, int count, Buffer &out)
   {
      NodeInput input = inputs[inputID];
      ObjectRef inputValue = input.node->getOutput(input.outputID, count);

      Vector<float> &output = object_cast<Vector<float> > (out[count]);
      if (inputValue->status != Object::valid)
      {
         output.status = inputValue->status;
         return;
      }
      int &in = dereference_cast<int> (inputValue);
      
      for (int i=0;i<outputLength;i++)
         output[i]=0;
      output[in]=1;
      
      output.status = Object::valid;
   }

};
