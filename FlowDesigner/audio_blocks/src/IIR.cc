// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <stdlib.h>

using namespace std;
using namespace FD;

class IIR;

DECLARE_NODE(IIR)
/*Node
 *
 * @name IIR
 * @category DSP:Filter
 * @description All-pole (IIR) filter
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input signal (frames)
 *
 * @input_name FILTER
 * @input_type Vector<float>
 * @input_description Filter coefficients (denominator)
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Filtered signal
 *
END*/


class IIR : public BufferedNode {
   
   int inputID;
   int outputID;
   int filterID;

public:
   IIR(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      filterID = addInput("FILTER");
      outputID = addOutput("OUTPUT");
      
   }

   virtual void initialize()
   {
      outputs[outputID].lookBack += 1;
      this->BufferedNode::initialize();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);
      ObjectRef filterValue = getInput(filterID, count);


      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;

      for (int i=0;i<inputLength;i++)
         output[i]=in[i];

      const Vector<float> &filter = object_cast<Vector<float> > (filterValue);
      const Vector<float> *past;
      bool can_look_back = false;
      if (count > 0)   
      {
         ObjectRef pastInputValue = this->getOutput(outputID, count-1);
	 can_look_back=true;
	 past = &object_cast<Vector<float> > (pastInputValue);
      }      
      

      int size = filter.size();

      if (can_look_back)
      {
         for (int i=0;i<size-1;i++)
         {
            int j, k;
            for (j=inputLength-1, k=1+i; k<size ; j--, k++)
            {
               output[i]-=(*past)[j]*filter[k];
            }         
         }
      }
      
      for (int i=0;i<inputLength;i++)
      {
         int j, k;
         for (j=i-1, k=1;j>=max(0,i-size+1) ; j--, k++)
            output[i]-=output[j]*filter[k];
      }
      for (int i=0;i<inputLength;i++)
         output[i]/=filter[0];
   }

};
