// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

using namespace std;

class MergeChannels;

DECLARE_NODE(MergeChannels)
/*Node

 * @name MergeChannels
 * @category DSP:Audio
 * @description No description available

 * @input_name LEFT
 * @input_description No description available

 * @input_name RIGHT
 * @input_description No description available

 * @output_name OUTPUT
 * @output_description No description available

END*/


class MergeChannels : public BufferedNode {
   
   int input1ID;
   int input2ID;
   int outputID;

public:
   MergeChannels(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      input1ID = addInput("LEFT");
      input2ID = addInput("RIGHT");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef input1Value = getInput(input1ID, count);


      ObjectRef input2Value = getInput(input2ID, count);


      const Vector<float> &in1 = object_cast<Vector<float> > (input1Value);
      const Vector<float> &in2 = object_cast<Vector<float> > (input2Value);
      
      if (in1.size() != in2.size())
      {
	 //cerr << in1.size() << " " << in2.size() << endl;
	 throw new NodeException (this, "Channels have different length", __FILE__, __LINE__);
      }
      int inputLength = in1.size();
      int outputLength = inputLength << 1;
      
      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;

      for (int i=0;i<inputLength;i++)
      {
	 output[2*i] = in1[i];
	 output[2*i+1] = in2[i];
      }
   }

};
