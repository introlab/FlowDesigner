// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <values.h>

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

class TimeEntropy;

DECLARE_NODE(TimeEntropy)
/*Node
 *
 * @name TimeEntropy
 * @category Signal:DSP
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name LOOKAHEAD
 * @parameter_description No description available
 *
 * @parameter_name LOOKBACK
 * @parameter_description No description available
 *
END*/


class TimeEntropy : public BufferedNode {
   
   int inputID;
   int outputID;

   int numberFrames;
   vector<Vector<float> *> frames;
   vector<float> min;

public:
   TimeEntropy(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)

   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      

      inputsCache[inputID].lookAhead = dereference_cast<int> (parameters.get("LOOKAHEAD"));
      inputsCache[inputID].lookBack = dereference_cast<int> (parameters.get("LOOKBACK"));
      
      numberFrames=inputsCache[inputID].lookBack+inputsCache[inputID].lookAhead+1;
      frames.resize(numberFrames);
      min.resize(numberFrames);
   }


   static inline float dist (float *in1, float *in2, int length)
   {
      int i;
      float sum=0;
      for (i=0;i<length;i++)
      {
         float tmp;
         tmp=in1[i]-in2[i];
         sum += tmp*tmp;
      }
      return sum;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      Vector<float> &output = *Vector<float>::alloc(1);
      out[count] = &output;


      if (count < inputsCache[inputID].lookBack)
      {
         output[0]=0.0;
         return;
      }
      NodeInput input = inputs[inputID];

      int i, j;
      int inputLength;
      for (i = -inputsCache[inputID].lookBack, j=0; i <= inputsCache[inputID].lookAhead ; i++, j++)
      {
         //RCPtr<Vector<float> > inputValue = input.node->getOutput(input.outputID, count + i);
         ObjectRef inputValue = input.node->getOutput(input.outputID, count + i);
         if (inputValue->status != Object::valid)
         {
            output[0]=0.0;
            return;
         }
         //frames[j] = inputValue.get();
         frames[j] = object_ptr_cast<Vector<float> *> (inputValue);
	 inputLength = frames[j]->size();
      }      
      
      //cerr << numberFrames << " " << (*(frames[0]))[0] << " " ; 

      for (i=0;i<numberFrames;i++)
         min[i]=FLT_MAX;
      
      /*float sx[inputLength];
      float sx2[inputLength];
      for (int i=0;i<inputLength;i++)
	 sx[i]=sx2[i]=0;
      */

      for (i=0;i<numberFrames;i++)
      {
	 /*for (j=0;j<inputLength;j++)
	 {
	    sx[j] += (*frames[i])[j];
	    sx2[j] += (*frames[i])[j] * (*frames[i])[j];
	 }
	 */
	 for (j=i+1;j<numberFrames;j++)
	 {
	    float tmp=dist(&(*frames[i])[0], &(*frames[j])[0], inputLength);
	    if (tmp < min[i]) min[i]=tmp;
	    if (tmp < min[j]) min[j]=tmp;
	 }
      }
      float accum=0;
      for (i=0;i<numberFrames;i++)
      {
         //cerr <<  min[i] << " ";
         accum += min[i];
      }
      output[0] = accum/numberFrames;
      //cerr << output[0] << endl;

      /*float var=0;
      for (j=0;j<inputLength;j++)
	 var += sx2[j]-sx[j]*sx[j]/numberFrames;
      output[1] = var/numberFrames/inputLength;
      */
   }

};
