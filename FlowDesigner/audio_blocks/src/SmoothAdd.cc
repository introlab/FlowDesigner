// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>

using namespace std;

class SmoothAdd;

DECLARE_NODE(SmoothAdd)
/*Node
 *
 * @name SmoothAdd
 * @category DSP:Manip
 * @description No description available
 *
 * @input_name LEFT
 * @input_description No description available
 *
 * @input_name CENTER
 * @input_description No description available
 *
 * @input_name RIGHT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name LENGTH
 * @parameter_description No description available
 *
END*/


class SmoothAdd : public BufferedNode {
   
   int input1ID;
   int input2ID;
   int input3ID;
   int outputID;
   int length;
   vector<float> hanning;

public:
   SmoothAdd(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      input1ID = addInput("LEFT");
      input2ID = addInput("CENTER");
      input3ID = addInput("RIGHT");
      outputID = addOutput("OUTPUT");

      length = dereference_cast<int> (parameters.get("LENGTH"));
   }

   virtual void initialize()
   {
      hanning.resize(length);
      for (int i=0;i<length;i++)
	 hanning[i] = .5-.5*cos((2*M_PI*i)/length);
      this->BufferedNode::initialize();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef input1Value = getInput(input1ID, count);

      ObjectRef input2Value = getInput(input2ID, count);

      ObjectRef input3Value = getInput(input3ID, count);
      
      const Vector<float> &in2 = object_cast<Vector<float> > (input2Value);
      
      out[count] = Vector<float>::alloc(length);
      Vector<float> &output = object_cast<Vector<float> > (out[count]);

      for (int i=0;i<length;i++)
      {
	 output[i]=hanning[i]*in2[i];
      }

      int half = length >> 1;

      const Vector<float> &in1 = object_cast<Vector<float> > (input1Value);
      for (int i=0;i<half;i++)
      {
	 output[i]+=(1.0-hanning[i])*in1[i];
      }

      const Vector<float> &in3 = object_cast<Vector<float> > (input3Value);
      for (int i=half;i<length;i++)
      {
	 output[i]+=(1-hanning[i])*in3[i];
      }
   }

};
