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
 * @parameter_name FAST
 * @parameter_type bool
 * @parameter_value true
 * @parameter_description If true, the DCT is implemented using an FFT
 *
 * @parameter_name OUTPUTLENGTH
 * @parameter_type int
 * @parameter_description Number of coefficients to calculate (only if FAST=false)
 *
END*/


// Details on this fast DCT algorithm at 
// http://mulan.eng.hmc.edu/~rwang/e186/handouts/dct/node3.html
class DCT : public BufferedNode {
   
   int inputID;
   int outputID;
   int length;
   bool fast;
   int outputLength;

   vector<float> rNormalize;
   vector<float> iNormalize;

public:
   DCT(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      length = dereference_cast<int> (parameters.get("LENGTH"));

      if (parameters.exist("FAST"))
	 fast = dereference_cast<bool> (parameters.get("FAST"));
      else
	 fast = true;

      if (parameters.exist("OUTPUTLENGTH"))
      {
	 if (fast)
	    throw new NodeException(NULL, "OUTPUTLENGTH can only be specified if FAST=false", __FILE__, __LINE__);
	 outputLength = dereference_cast<int> (parameters.get("OUTPUTLENGTH"));
      } else 
	 outputLength = length;

      if (fast)
      {
	 rNormalize.resize(length);
	 iNormalize.resize(length);
	 float sqrt2n=sqrt(2.0/length);
	 for (int i=0;i<length;i++)
	 {
	    rNormalize[i]=cos(M_PI*i/(2*length))*sqrt2n;
	    iNormalize[i]=-sin(M_PI*i/(2*length))*sqrt2n;
	 }
	 rNormalize[0] /= sqrt(2.0);
      } else {
	 float sqrt2n=sqrt(2.0/length);
	 rNormalize.resize(length*outputLength);

	 float c1 = M_PI / length;
	 int counter = 0;
	 for (int j = 0; j < length; j++)
	    rNormalize[counter++] = sqrt2n*sqrt(.5);
	 for (int i = 1; i < outputLength; i++) {
	    float c2 = c1 * i;
	    for (int j = 0; j < length; j++) {
	       float x = (j + 0.5) * c2;
	       rNormalize[counter++] = sqrt2n*cos(x);
	    }
	 }
	 
      }

   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;
      
      if (fast)
      {
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
      } else {
	 
	 int i, j;
	 float sum;
	 float *ptr = &rNormalize[0];

	 for (i = 0; i < outputLength; i++) {
	    sum = 0.0;
	    for (j = 0; j < length; j++, ptr++)
	       sum += in[j] * (*ptr);
	    output[i] = sum;
	 }
	 
      }
   }

NO_ORDER_NODE_SPEEDUP(DCT)
};
