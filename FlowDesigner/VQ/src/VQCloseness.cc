// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "kmeans.h"
#include <math.h>

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif
#ifdef WIN32
#include <float.h>
#endif
using namespace std;

namespace FD {

class VQCloseness;

DECLARE_NODE(VQCloseness)
/*Node
 *
 * @name VQCloseness
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
 *
END*/


class VQCloseness : public BufferedNode {
   
   int inputID;
   int VQinputID;
   int outputID;

public:
   VQCloseness(string nodeName, ParameterSet params)
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


      ObjectRef inputValue = input.node->getOutput(input.outputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();
      const KMeans &vq = object_cast<KMeans> (VQValue);

      int outputLength = vq.nbClasses();
      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;

      
      vq.calcDist(&in[0], &output[0]);

      float sum = 10*FLT_MIN;
      float epsilon = 100*FLT_MIN;
      for (int i=0;i<outputLength;i++)
      {
	 //float tmp = 1/((output[i]*output[i]*output[i]*output[i])+epsilon);
         float tmp = exp(-output[i]);
	 output[i]=tmp;
	 sum += tmp;
      }
      for (int i=0;i<outputLength;i++)
      {
	 output[i] /= sum;
	 //output[i] = 2*output[i] - 1;
      }
   }

};
}//namespace FD
