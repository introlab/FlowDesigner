// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "NNetSet.h"

class NNetSetCalc;

DECLARE_NODE(NNetSetCalc)
/*Node
 *
 * @name NNetSetCalc
 * @category NNet
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @input_name ID
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


class NNetSetCalc : public BufferedNode {
   
   int inputID;
   int netInputID;
   int IDInputID;
   int outputID;
   int outputLength;

public:
   NNetSetCalc(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      netInputID = addInput("NNET");
      IDInputID = addInput("ID");
      outputID = addOutput("OUTPUT");

      outputLength = dereference_cast<int> (parameters.get("OUTPUTLENGTH"));
      

   }

   void calculate(int output_id, int count, Buffer &out)
   {

      ObjectRef inputValue = getInput(inputID, count);
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      ObjectRef netValue = getInput(netInputID, count);
      NNetSet &net = object_cast<NNetSet> (netValue);
      
      ObjectRef idValue = getInput(IDInputID, count);
      const Vector<float> &id = object_cast<Vector<float> > (idValue);

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;

      float *netOut = net.calc(int(floor(id[0])), &in[0]);
      
      for (int i=0;i<outputLength;i++)
         output[i]=netOut[i];
   }

};
