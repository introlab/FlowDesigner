// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>

#ifdef HAVE_VALUES_H
#include <values.h>
#endif

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

using namespace std;
using namespace FD;

class Entropy;

DECLARE_NODE(Entropy)
/*Node
 *
 * @name Entropy
 * @category DSP:Misc
 * @description Calculates the entropy of a vector
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input vector
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Entropy value (vector of 1)
 *
END*/


class Entropy : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   Entropy(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)

   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(1);
      out[count] = &output;

      float s2=0;
      float entr=0;
      for (int i=0;i<inputLength;i++)
      {
         s2+=in[i]*in[i];
      }
      s2 = 1/s2;

      for (int i=0;i<inputLength;i++)
      {
	 if (in[i] != 0)
	    entr -= s2*in[i]*in[i] * log(s2*in[i]*in[i]);
      }
      //cout << entr << endl;
      output[0] = entr;
   }

};
