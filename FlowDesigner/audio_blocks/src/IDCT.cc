// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>
#include "FFTWrap.h"

class IDCT;

DECLARE_NODE(IDCT)
/*Node
 *
 * @name IDCT
 * @category Signal:DSP
 * @require FFT
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name LENGTH
 * @parameter_description No description available
 *
END*/


class IDCT : public BufferedNode {
   
   int inputID;
   int outputID;
   int length;

   vector<complex<float> > inputCopy;
   vector<complex<float> > outputCopy;
   vector<float> rNormalize;
   vector<float> iNormalize;

public:
   IDCT(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)

   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      length = dereference_cast<int> (parameters.get("LENGTH"));

      if (length & 1) 
      {
	 throw new NodeException(NULL, "IDCT only implemented for even sizes", __FILE__, __LINE__);
      }

      inputCopy.resize(length);
      outputCopy.resize(length);
      rNormalize.resize(length);
      iNormalize.resize(length);
      float sqrt2n=sqrt(2.0/length);
      for (int i=0;i<length;i++)
      {
	 rNormalize[i]=cos(M_PI*i/(2*length))*sqrt2n;
	 iNormalize[i]=sin(M_PI*i/(2*length))*sqrt2n;
      }
      rNormalize[0] /= sqrt(2);

   }


   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
         return;
      }
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);

      Vector<float> &output = *Vector<float>::alloc(length);
      out[count] = &output;
            
      int i,j;


      for (i=0;i<length;i++)
      {
	 inputCopy[i] = complex<float> (rNormalize[i]*in[i], iNormalize[i]*in[i]);
      }

      FFTWrap.ifft(&inputCopy[0], &outputCopy[0], length);

      for (i=0, j=0 ;i<length ; i+=2, j++)
         output[i]=outputCopy[j].real();

      for (i = length-1; i>=0 ; i-=2, j++)
         output[i]=outputCopy[j].real();
   }

};
