// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "kmeans.h"

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

using namespace std;

class VQWeightMeans;

DECLARE_NODE(VQWeightMeans)
/*Node
 *
 * @name VQWeightMeans
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
 * @parameter_name OUTPUTLENGTH
 * @parameter_description No description available
 *
END*/


class VQWeightMeans : public BufferedNode {
   
   int inputID;
   int VQinputID;
   int outputID;
   int outputLength;

public:
   VQWeightMeans(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      VQinputID = addInput("VQ");
      outputID = addOutput("OUTPUT");
      outputLength = dereference_cast<int> (parameters.get("OUTPUTLENGTH"));
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef VQValue = getInput(VQinputID, count);

      ObjectRef inputValue = getInput(inputID, count);

      const KMeans &vq = object_cast<KMeans> (VQValue);
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;

      
      vq.weightMeans(in, output);
   }

};
