// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>
#include "FFTWrap.h"

using namespace std;

class IDCT;

DECLARE_NODE(IDCT)
/*Node
 *
 * @name IDCT
 * @category DSP:TimeFreq
 * @require FFT
 * @description Fast implementation of the inverse discrete cosine transform (IDCT) using an FFT
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description The input vector
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description The result of the DCT
 *
 * @parameter_name LENGTH
 * @parameter_type int
 * @parameter_description Length of the input and output vectors
 *
END*/

// Details on this fast inverse DCT algorithm at 
// http://mulan.eng.hmc.edu/~rwang/e186/handouts/dct/node4.html
class IDCT : public BufferedNode {
   
   int inputID;
   int outputID;
   int length;

   vector<float> rNormalize;
   vector<float> iNormalize;

public:
   IDCT(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)

   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      length = dereference_cast<int> (parameters.get("LENGTH"));

      rNormalize.resize(length);
      iNormalize.resize(length);
      float sqrt2n=sqrt(2.0/length);
      for (int i=0;i<length;i++)
      {
	 rNormalize[i]=cos(M_PI*i/(2*length))*sqrt2n;
	 iNormalize[i]=sin(M_PI*i/(2*length))*sqrt2n;
      }
      rNormalize[0] /= sqrt(2.0);

   }


   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);

      Vector<float> &output = *Vector<float>::alloc(length);
      out[count] = &output;
            
      int i,j;

      DYN_VEC(float, length, inputCopy);
      DYN_VEC(float, length, outputCopy);
      inputCopy[0]=rNormalize[0]*in[0];
      
      //Y(n) = X(n)*exp(j*n*pi/2N) and transform the result so we have a symetric (conjugate) spectrum
      for (i=1;i<(length+1)>>1;i++)
      {
	 inputCopy[i] = .5*(rNormalize[i]*in[i]+rNormalize[length-i]*in[length-i]);
	 inputCopy[length-i] = .5*(iNormalize[i]*in[i] - iNormalize[length-i]*in[length-i]);
      }
      if (!(length&1))
	 inputCopy[length>>1] = rNormalize[length>>1]*in[length>>1];

      //Real inverse FFT (that's why having a symetric spectrum saved a factor of two)
      FFTWrap.irfft(inputCopy, outputCopy, length);

      /*Re-arrange the vector*/
      for (i=0, j=0 ;i<length ; i+=2, j++)
         output[i]=outputCopy[j];

      for (j=length-1,i=1;i<length;i+=2,j--)
	 output[i]=outputCopy[j];
   }

};
