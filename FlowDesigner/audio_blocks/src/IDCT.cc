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

   vector<float> inputCopy;
   vector<float> outputCopy;
   vector<float> rNormalize;
   vector<float> iNormalize;

public:
   IDCT(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)

   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      length = dereference_cast<int> (parameters.get("LENGTH"));

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

      inputCopy[0]=rNormalize[0]*in[0];
      
      for (i=1;i<(length+1)>>1;i++)
      {
	 inputCopy[i] = .5*(rNormalize[i]*in[i]+rNormalize[length-i]*in[length-i]);
	 inputCopy[length-i] = .5*(iNormalize[i]*in[i] - iNormalize[length-i]*in[length-i]);
      }
      if (!(length&1))
	 inputCopy[length>>1] = rNormalize[length>>1]*in[length>>1];

      for (i=0;i<length;i++)
	 cout << inputCopy[i] << " ";
      cout << endl;

      FFTWrap.irfft(&inputCopy[0], &outputCopy[0], length);

      for (i=0;i<length;i++)
	 cout << outputCopy[i] << " ";
      cout << endl;

      for (i=0, j=0 ;i<length ; i+=2, j++)
         output[i]=outputCopy[j];

      for (i = length-1; i>=0 ; i-=2, j++)
         output[i]=outputCopy[j];
   }

};
