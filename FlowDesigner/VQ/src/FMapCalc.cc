// Copyright (C) 1999 Jean-Marc Valin

#include <stream.h>
#include "FrameOperation.h"
#include "Buffer.h"
#include "Vector.h"
#include "FeatureMap.h"

class FMapCalc;

DECLARE_NODE(FMapCalc)
/*Node

 * @name FMapCalc
 * @category VQ
 * @description No description available

 * @input_name INPUT
 * @input_description No description available

 * @input_name FMAP
 * @input_description No description available

 * @output_name OUTPUT
 * @output_description No description available

 * @parameter_name INPUTLENGTH
 * @parameter_description No description available

 * @parameter_name OUTPUTLENGTH
 * @parameter_description No description available

END*/


class FMapCalc : public FrameOperation {
   
   int inputID;
   int netInputID;
   int inputLength;

public:
   FMapCalc(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      inputID = addInput("INPUT");
      netInputID = addInput("FMAP");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH")); 
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));
   }

   ~FMapCalc() {}

   virtual void specificInitialize()
   {
      this->FrameOperation::specificInitialize();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      NodeInput input = inputs[inputID];
      NodeInput netInput = inputs[netInputID];

      ObjectRef netValue = netInput.node->getOutput(netInput.outputID, count);

      Vector<float> &output = object_cast<Vector<float> > (out[count]);
      if (netValue->status != Object::valid)
      {
         output.status = netValue->status;
         return;
      }

      ObjectRef inputValue = input.node->getOutput(input.outputID, count);

      if (inputValue->status != Object::valid)
      {
         output.status = inputValue->status;
         return;
      }
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      FeatureMap &fmap = object_cast<FeatureMap> (netValue);
      
      /*double tmp[in.size()];
      for (int i=0;i<in.size();i++)
	 tmp[i]=in[i];
      double *netOut = net.calc(tmp);
      
      for (int i=0;i<outputLength;i++)
         output[i]=netOut[i];
      */
      fmap.calc(in.begin(), output.begin());

      output.status = Object::valid;
   }

};
