// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

class DownSample;

DECLARE_NODE(DownSample)
/*Node
 *
 * @name DownSample
 * @category DSP:Base
 * @description Downsamples a signal by outputing one sample for every N input samples
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Downsampling input
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Downsampled (by N) output
 *
 * @parameter_name FACTOR
 * @parameter_type int
 * @parameter_description Downsampling factor N
 *
END*/


class DownSample : public BufferedNode {
   
   int inputID;
   int outputID;
   int factor;

public:
   DownSample(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      factor = dereference_cast<int> (parameters.get("FACTOR"));
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();
      int outputLength = inputLength/factor;

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;
      
      int i,j;
      for (i=0,j=0;i<outputLength;i++,j+=factor)
      {
         output[i]=in[j];
      }
   }

};
