// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <complex>

class Conj;

DECLARE_NODE(Conj)
/*Node
 *
 * @name Conj
 * @category DSP:Base
 * @description Computes the complex conjugate of a vector
 *
 * @input_name INPUT
 * @input_type Vector<complex>
 * @input_description Input vector
 *
 * @output_name OUTPUT
 * @output_type Vector<complex>
 * @output_description Conjugate vector
 *
END*/


class Conj : public BufferedNode {
   
   int inputID;
   int outputID;
   float gain;

public:
   Conj(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<complex<float> > &in = object_cast<Vector<complex<float> > > (inputValue);
      int inputLength = in.size();

      Vector<complex<float> > &output = *Vector<complex<float> >::alloc(inputLength);
      out[count] = &output;

      for (int i=0;i<inputLength;i++)
	 output[i] = conj(in[i]);
      
   }

      
};
