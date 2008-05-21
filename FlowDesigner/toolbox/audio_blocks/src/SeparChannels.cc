// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Vector.h"

using namespace std;

namespace FD {

class SeparChannels;

DECLARE_NODE(SeparChannels);
/*Node
 *
 * @name SeparChannels
 * @category DSP:Audio
 * @description No description available
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Stero frame (encoded as left, right, left, right, ...)
 *
 * @output_name CHANNEL1
 * @output_type Vector<float>
 * @output_description Frame for first channel
 *
 * @output_name CHANNEL2
 * @output_type Vector<float>
 * @output_description Frame for second channel
 *
 * @output_name CHANNEL3
 * @output_type Vector<float>
 * @output_description Frame for 3rd channel
 *
 * @output_name CHANNEL4
 * @output_type Vector<float>
 * @output_description Frame for 4th channel
 *
 * @output_name CHANNEL5
 * @output_type Vector<float>
 * @output_description Frame for 5th channel
 *
 * @output_name CHANNEL6
 * @output_type Vector<float>
 * @output_description Frame for 6th channel
 *
 * @output_name CHANNEL7
 * @output_type Vector<float>
 * @output_description Frame for 7th channel
 *
 * @output_name CHANNEL8
 * @output_type Vector<float>
 * @output_description Frame for 8th channel
 *
 * @parameter_name NB_CHANNELS
 * @parameter_type int
 * @parameter_value 2
 * @parameter_description Number of channels in the input
 *
END*/


class SeparChannels : public BufferedNode {
   
   int inputID;
   vector<int> outputID;

public:
   SeparChannels(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      if (parameters.exist("NB_CHANNELS"))
      {
         outputID.resize(dereference_cast<int> (parameters.get("NB_CHANNELS")));
         for (int i=0;i<outputID.size();i++)
         {
            char inStr[9] = "CHANNELX";
            inStr[7] = '1'+i;
            outputID[i] = addOutput(inStr);
         }
      } else {
         outputID.resize(2);
         outputID[0] = addOutput("LEFT");
         outputID[1] = addOutput("RIGHT");
      }
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();
      int outputLength = inputLength/outputID.size();

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;
      
      int channel;
      for (int i=0;i<outputID.size();i++)
         if (output_id == outputID[i])
            channel = i;
      if (channel>=outputID.size())
         throw new NodeException(this, "Sound copy constructor should not be called",__FILE__,__LINE__);
      for (int i=0;i<outputLength;i++)
      {
         output[i] = in[outputID.size()*i+channel];
      }
   }

};

}//namespace FD

