// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>
#include "FFTWrap.h"

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

class MFCC;

DECLARE_NODE(MFCC)
/*Node
 *
 * @name MFCC
 * @category ZDeprecated
 * @description Calculates MFCC coefficients from an audio frame (all in one)
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name INPUTLENGTH
 * @parameter_description No description available
 *
 * @parameter_name OUTPUTLENGTH
 * @parameter_description No description available
 *
 * @parameter_name WINDOW
 * @parameter_description No description available
 *
 * @parameter_name SAMPLING
 * @parameter_description No description available
 *
 * @parameter_name LOW
 * @parameter_description No description available
 *
 * @parameter_name HIGH
 * @parameter_description No description available
 *
END*/


class MFCC : public BufferedNode {
   
   int inputID;
   int outputID;
   int inputLength;
   int outputLength;
   vector<vector<float> > filters;
   vector<int> filterStart;
   vector<float> window;
   int psLength;
   int melLength;

   float *inputCopy;
   float *outputCopy;
   float *tmpBuffer1;
   float *tmpBuffer2;
   float *rNormalize;
   float *iNormalize;


public:
   MFCC(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      try {
         inputID = addInput("INPUT");
	 outputID = addOutput("OUTPUT");

	 inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
	 outputLength = dereference_cast<int> (parameters.get("OUTPUTLENGTH"));
         
         psLength = inputLength/2;
	 melLength = outputLength;
         
         filters.resize(outputLength);
         filterStart.resize(outputLength);
         window.resize(inputLength);
      } catch (BaseException *e)
      {
         throw e->add(new NodeException (NULL, "Exception caught in MFCC constructor", __FILE__, __LINE__));
      }
      
   }

   ~MFCC() 
   {
      delete [] inputCopy;
      delete [] outputCopy;
      delete [] rNormalize;
      delete [] iNormalize;
      delete [] tmpBuffer1;
      delete [] tmpBuffer2;
   }

   virtual void specificInitialize()
   {
      int i;

      this->BufferedNode::specificInitialize();


      String type = object_cast<String> (parameters.get("WINDOW"));
      if (type == "HANNING")
      {
         for (i=0;i<inputLength;i++)
            window[i]=.5-.5*cos((2*M_PI*i)/inputLength);
      } else if (type == "HAMMING")
      {
         for (i=0;i<inputLength;i++)
            window[i]=.54-.46*cos((2*M_PI*i)/inputLength);
      } else 
      {
         throw new GeneralException("Unknown window type",__FILE__,__LINE__);
      }


      tmpBuffer1 = new float [inputLength];
      tmpBuffer2 = new float [inputLength];

      float niquist = dereference_cast<int> (parameters.get("SAMPLING")) / 2.0;
      float high = dereference_cast<int> (parameters.get("HIGH"));
      float low = dereference_cast<int> (parameters.get("LOW"));
      float highMel = 1000*log(1+high/700)/log(1+1000.0/700);
      float lowMel = 1000*log(1+low/700)/log(1+1000.0/700);
      vector<int> centers(melLength+2);

      for (i=0;i<melLength+2;i++)
      {
         float melCenter = lowMel + i*(highMel-lowMel)/(melLength+1);
         centers[i] = int (floor(.5 + psLength*700*(exp(melCenter*log(1+1000.0/700.0)/1000)-1)/niquist));
      }
      for (i=0;i<melLength;i++)
      {
         filterStart[i] = centers[i]+1;
         filters[i].resize(centers[i+2]-centers[i]-1);
         int freq, j;
         for (freq=centers[i]+1, j=0 ; freq<=centers[i+1]; freq++, j++)
         {
            filters[i][j] = (freq-centers[i])/float(centers[i+1]-centers[i]);
         }
         for (freq=centers[i+1]+1 ; freq < centers[i+2] ; freq++, j++)
         {
            filters[i][j] = (centers[i+2]-freq)/float(centers[i+2]-centers[i+1]);
         }
      }
      

      inputCopy = new float [melLength];
      outputCopy =new float [melLength];
      rNormalize =new float [melLength];
      iNormalize =new float [melLength];
      float sqrt2n=sqrt(2.0/inputLength);
      for (int i=0;i<melLength;i++)
      {
	 rNormalize[i]=cos(M_PI*i/(2*melLength))*sqrt2n;
	 iNormalize[i]=-sin(M_PI*i/(2*melLength))*sqrt2n;
      }
      rNormalize[0] /= sqrt(2);

   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      
      if (in.size() != inputLength)
	 throw new NodeException(this, "Size of input != size of window", __FILE__, __LINE__);

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;


      int i,j;
      for (i=0;i<inputLength;i++)
         tmpBuffer1[i]=in[i]*window[i];

      FFTWrap.rfft(tmpBuffer1, tmpBuffer2, inputLength);

      tmpBuffer2[0]=tmpBuffer2[0]*tmpBuffer2[0];
      for (i=1;i<psLength;i++)
      {
         tmpBuffer2[i] = tmpBuffer2[i]*tmpBuffer2[i]
         + tmpBuffer2[inputLength-i]*tmpBuffer2[inputLength-i];
      }

      
      int nbFilters = filters.size();
      for (i = 0 ; i < nbFilters ; i++)
      {
         int j;
         tmpBuffer1[i]=0;
         int filterSize = filters[i].size();
         int filtStart = filterStart[i];
         for (j=0;j<filterSize;j++)
         {
            tmpBuffer1[i] += filters[i][j]*tmpBuffer2[j+filtStart];
         }
      }
      

      for (i=0, j=0 ;i<melLength ; i+=2, j++)
         inputCopy[j]=log(tmpBuffer1[i]+FLT_MIN);

      for (i = melLength-1; i>=0 ; i-=2, j++)
         inputCopy[j]=log(tmpBuffer1[i]+FLT_MIN);
      

      FFTWrap.rfft(inputCopy, outputCopy, melLength);
      
      for (i=1;i<melLength/2;i++)
      {
	 output[i]=rNormalize[i]*outputCopy[i] - iNormalize[i]*outputCopy[melLength-i];
	 output[melLength-i]=rNormalize[melLength-i]*outputCopy[i] + iNormalize[melLength-i]*outputCopy[melLength-i];
      }

      output[0]=outputCopy[0]*rNormalize[0];
      output[melLength/2] = outputCopy[melLength/2]*rNormalize[melLength/2];
   }

};
