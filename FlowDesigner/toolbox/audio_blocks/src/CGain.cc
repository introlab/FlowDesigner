// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

using namespace std;

namespace FD {

class CGain;

DECLARE_NODE(CGain)
/*Node

 * @name CGain
 * @category DSP:Base
 * @description No description available

 * @input_name INPUT
 * @input_description No description available

 * @input_name GAIN
 * @input_description No description available

 * @output_name OUTPUT
 * @output_description No description available

END*/


class CGain : public BufferedNode {
   
   int inputID;
   int outputID;
   int gainID;

public:
   CGain(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      gainID = addInput("GAIN");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;

      ObjectRef gainValue = getInput(gainID, count);

      const Vector<float> &gain = object_cast<Vector<float> > (gainValue);

      float g=gain[0];

      for (int i=0;i<inputLength;i++)
      {
         output[i]=g*in[i];
      }
   }

};
}//namespace FD
