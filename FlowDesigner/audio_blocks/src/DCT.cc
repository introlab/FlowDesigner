// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>
#include "FFTWrap.h"

class DCT;

DECLARE_NODE(DCT)
/*Node
 *
 * @name DCT
 * @category DSP:TimeFreq
 * @require FFT
 * @description Fast implementation of the discrete cosine transform (DCT) using an FFT
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
 * @parameter_description Length of the DCT
 *
END*/


// Details on this fast DCT algorithm at 
// http://mulan.eng.hmc.edu/~rwang/e186/handouts/dct/node3.html
class DCT : public BufferedNode {
   
   int inputID;
   int outputID;
   int length;

   vector<float> rNormalize;
   vector<float> iNormalize;

public:
   DCT(string nodeName, ParameterSet params)
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
	 iNormalize[i]=-sin(M_PI*i/(2*length))*sqrt2n;
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
      
      DYN_VEC(float, length, inputCopy);
      DYN_VEC(float, length, outputCopy);
      int i,j;
      //Re-order the vector for the DCT (see reference)
      for (i=0, j=0 ;i<length ; i+=2, j++)
         inputCopy[j]=in[i];

      for (j=length-1,i=1;i<length;i+=2,j--)
	 inputCopy[j]=in[i];

      //Real FFT
      FFTWrap.rfft(inputCopy, outputCopy, length);

      //X(n) = Re[ Y(n) * exp(-j*n*pi/2N) ]
      output[0]=outputCopy[0]*rNormalize[0];
      for (i=1;i<(length+1)>>1;i++)
      {
	 output[i]=rNormalize[i]*outputCopy[i] - iNormalize[i]*outputCopy[length-i];
	 output[length-i]=rNormalize[length-i]*outputCopy[i] + iNormalize[length-i]*outputCopy[length-i];
      }
      if (!(length&1)) //even case
      {
	 output[length>>1] = outputCopy[length>>1]*rNormalize[length>>1];
      }
      
   }

};
