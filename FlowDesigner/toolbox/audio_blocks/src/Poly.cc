// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

using namespace std;

namespace FD {

class Poly;

DECLARE_NODE(Poly)
/*Node
 *
 * @name Poly
 * @category DSP:Base
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @input_name COEF
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
END*/


class Poly : public BufferedNode {
   
   int input1ID;
   int input2ID;
   int outputID;

public:
   Poly(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      input1ID = addInput("INPUT");
      input2ID = addInput("COEF");
      outputID = addOutput("OUTPUT");

   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef input1Value = getInput(input1ID, count);


      ObjectRef input2Value = getInput(input2ID, count);


      const Vector<float> &in1 = object_cast<Vector<float> > (input1Value);
      const Vector<float> &in2 = object_cast<Vector<float> > (input2Value);
      int inputLength = in1.size();

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;
      
      for (int i=0;i<inputLength;i++)
      {
	 float x_n = 1;
	 output[i] = 0;
	 for (int j=0;j<in2.size();j++)
	 {
	    output[i] += in2[j] * x_n;
	    x_n *= in1[i];
	 }
      }
   }

};

}//namespace FD
