// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "FFNet.h"

class NNetExec;

DECLARE_NODE(NNetExec)
/*Node
 *
 * @name NNetExec
 * @category NNet
 * @description No description available
 * @require FFNet
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @input_name NNET
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name OUTPUTLENGTH
 * @parameter_description No description available
 *
END*/


class NNetExec : public BufferedNode {
   
   int inputID;
   int netInputID;
   int outputID;
   int outputLength;

public:
   NNetExec(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      netInputID = addInput("NNET");
      outputID = addOutput("OUTPUT");

      outputLength = dereference_cast<int> (parameters.get("OUTPUTLENGTH")); 
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef netValue = getInput(netInputID, count);

      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;

      FFNet &net = object_cast<FFNet> (netValue);
      
      //float value[net.getNbNeurons()];
      DYN_VEC(float, net.getNbNeurons(), value);
      
      float *netOut = net.calc(&in[0], value);

      /*double tmp[in.size()];
      for (int i=0;i<in.size();i++)
	 tmp[i]=in[i];
      double *netOut = net.calc(tmp);
      */

      for (int i=0;i<outputLength;i++)
         output[i]=netOut[i];
   }

};
