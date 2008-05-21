// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

using namespace std;

namespace FD {

class MergeChannels;

DECLARE_NODE(MergeChannels);
/*Node
 *
 * @name MergeChannels
 * @category DSP:Audio
 * @description No description available
 *
 * @input_name CHANNEL1
 * @input_description No description available
 *
 * @input_name CHANNEL2
 * @input_description No description available
 *
 * @input_name CHANNEL3
 * @input_description No description available
 *
 * @input_name CHANNEL4
 * @input_description No description available
 *
 * @input_name CHANNEL5
 * @input_description No description available
 *
 * @input_name CHANNEL6
 * @input_description No description available
 *
 * @input_name CHANNEL7
 * @input_description No description available
 *
 * @input_name CHANNEL8
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name NB_CHANNELS
 * @parameter_type int
 * @parameter_value 2
 * @parameter_description Number of channels in the input
 *
 * @parameter_name ADDING
 * @parameter_type bool
 * @parameter_value false
 * @parameter_description Whether the channels should be added instead of interlaced
 *
END*/


class MergeChannels : public BufferedNode {
   
   vector<int> inputID;
   int outputID;
   bool adding;
public:
   MergeChannels(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      if (parameters.exist("NB_CHANNELS"))
      {
         inputID.resize(dereference_cast<int> (parameters.get("NB_CHANNELS")));
         for (int i=0;i<inputID.size();i++)
         {
            char inStr[9] = "CHANNELX";
            inStr[7] = '1'+i;
            inputID[i] = addInput(inStr);
         }
      } else {
         inputID.resize(2);
         inputID[0] = addInput("LEFT");
         inputID[1] = addInput("RIGHT");
      }

      outputID = addOutput("OUTPUT");
      
      adding = false;
      if (parameters.exist("ADDING") && dereference_cast<bool> (parameters.get("ADDING")))
         adding = true;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      vector<ObjectRef> inputValues(inputID.size());
      
      for (int i=0;i<inputID.size();i++)
         inputValues[i] = getInput(inputID[i], count);
      
      vector<const Vector<float> *> in (inputID.size());
      for (int i=0;i<inputID.size();i++)
      {
         in[i] = &object_cast<Vector<float> > (inputValues[i]);
         if (in[i]->size() != in[0]->size())
            throw new NodeException (this, "Channels have different length", __FILE__, __LINE__);
      }
      int inputLength = in[0]->size();
      int outputLength;
      
      if (adding)
         outputLength = inputLength;
      else
         outputLength = inputLength*inputID.size();
      
      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;

      if (adding)
      {
         for (int i=0;i<inputLength;i++)
            output[i] = 0;
         for (int j=0;j<inputID.size();j++)
         {
            for (int i=0;i<inputLength;i++)
               output[i] += (*(in[j]))[i];
         }         
      } else {
         for (int j=0;j<inputID.size();j++)
         {
            for (int i=0;i<inputLength;i++)
               output[inputID.size()*i+j] = (*(in[j]))[i];
         }
      }
   }

};
}//namespace FD
