// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "kmeans.h"

class VQClass;

DECLARE_NODE(VQClass)
/*Node
 *
 * @name VQClass
 * @category VQ
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @input_name VQ
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
END*/


class VQClass : public BufferedNode {
   
   int inputID;
   int VQinputID;
   int outputID;
      //int inputLength;

public:
   VQClass(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      VQinputID = addInput("VQ");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      NodeInput input = inputs[inputID];
      NodeInput VQInput = inputs[VQinputID];

      ObjectRef VQValue = VQInput.node->getOutput(VQInput.outputID, count);

      if (VQValue->status != Object::valid)
      {
         out[count] = VQValue;
         return;
      }

      ObjectRef inputValue = input.node->getOutput(input.outputID, count);

      if (inputValue->status != Object::valid)
      {
         out[count] = inputValue;
         return;
      }
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      const KMeans &vq = object_cast<KMeans> (VQValue);
      
      int classID = vq.getClassID(&in[0]);
      //const vector<float> &mean = vq[classID];

      //for (int i=0;i<outputLength;i++)
      //   output[i]=mean[i];
      

      Vector<float> &output = *Vector<float>::alloc(1);
      out[count] = &output;

      output[0] = classID;

   }

};
