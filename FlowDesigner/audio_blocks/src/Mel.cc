// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>

class Mel;

DECLARE_NODE(Mel)
/*Node
 *
 * @name Mel
 * @category DSP:TimeFreq
 * @description calculates Mel-scale channel energies from power-spectrum
 *
 * @input_name INPUT
 * @input_type Vector
 * @input_description Input power-spectrum
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description Mel-scale channel energies
 *
 * @parameter_name INPUTLENGTH
 * @parameter_type int
 * @parameter_description Power-spectrum size
 *
 * @parameter_name OUTPUTLENGTH
 * @parameter_type int
 * @parameter_description Number of channel energies
 *
 * @parameter_name SAMPLING
 * @parameter_type int
 * @parameter_description Sampling rate used (used for power-spectrum range)
 *
 * @parameter_name LOW
 * @parameter_type int
 * @parameter_description Lowest frequency
 *
 * @parameter_name HIGH
 * @parameter_type int
 * @parameter_description Highest frequency
 *
END*/


class Mel : public BufferedNode {
   
   int inputID;
   int outputID;
   int inputLength;
   int outputLength;
   vector<vector<float> > filters;
   vector<int> filterStart;

public:
   Mel(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      outputLength = dereference_cast<int> (parameters.get("OUTPUTLENGTH"));

      filters.resize(outputLength);
      filterStart.resize(outputLength);
   }

   virtual void specificInitialize()
   {
      this->BufferedNode::specificInitialize();
      float niquist = dereference_cast<int> (parameters.get("SAMPLING")) / 2.0;
      float high = dereference_cast<int> (parameters.get("HIGH"));
      float low = dereference_cast<int> (parameters.get("LOW"));
      float highMel = 1000*log(1+high/700)/log(1+1000.0/700);
      float lowMel = 1000*log(1+low/700)/log(1+1000.0/700);
      vector<int> centers(outputLength+2);
      int i;
      for (i=0;i<outputLength+2;i++)
      {
         float melCenter = lowMel + i*(highMel-lowMel)/(outputLength+1);
         centers[i] = int (floor(.5 + inputLength*700*(exp(melCenter*log(1+1000.0/700.0)/1000)-1)/niquist));
      }
      
      for (i=0;i<outputLength;i++)
      {
         filterStart[i] = centers[i]+1;
         filters[i].resize(centers[i+2]-centers[i]);
         int freq, j;
         for (freq=centers[i]+1, j=0 ; freq<centers[i+1]; freq++, j++)
         {
            filters[i][j] = (freq-centers[i])/float(centers[i+1]-centers[i]);
         }
         for (freq=centers[i+1] ; freq < centers[i+2] ; freq++, j++)
         {
            filters[i][j] = (centers[i+2]-freq)/float(centers[i+2]-centers[i+1]);
         }
      }
      
      
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);

      if (in.size() != inputLength)
	 throw new NodeException(this, "Input size mismatch", __FILE__, __LINE__);

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;

      
      int i;
      int nbFilters = filters.size();
      for (i = 0 ; i < nbFilters ; i++)
      {
         int j;
         output[i]=0;
         int filterSize = filters[i].size();
         int filtStart = filterStart[i];
         for (j=0;j<filterSize;j++)
         {
            output[i] += filters[i][j]*in[j+filtStart];
         }
      }
      
   }

};
