// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <stdlib.h>
#include <math.h>

class ILTF;

DECLARE_NODE(ILTF)
/*Node
 *
 * @name ILTF
 * @category Signal:DSP
 * @description Inverse (all-pole) long-term (comb) filter
 *
 * @input_name INPUT
 * @input_type Vector
 * @input_description Input frame
 *
 * @input_name FILTER
 * @input_type Vector
 * @input_description Filter params as [gain, period]
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description Filtered signal
 *
END*/


class ILTF : public BufferedNode {
   
   int inputID;
   int outputID;
   int filterID;
   int noncausal;
   bool continuous;

public:
   ILTF(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      filterID = addInput("FILTER");
      outputID = addOutput("OUTPUT");
   }

   virtual void specificInitialize()
   {
      outputs[outputID].lookBack += 1;
      this->BufferedNode::specificInitialize();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);
      ObjectRef filterValue = getInput(filterID, count);

      if (inputValue->status != Object::valid)
      {
         out[count] = inputValue;
         return;
      }

      if (filterValue->status != Object::valid)
      {
         out[count] = filterValue;
         return;
      }

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int length = in.size();
      const Vector<float> &filter = object_cast<Vector<float> > (filterValue);
      const Vector<float> *past;
      bool can_look_back = false;

      out[count] = Vector<float>::alloc(length);
      Vector<float> &output = object_cast<Vector<float> > (out[count]);
      if (count > 0)   
      {
         ObjectRef pastInputValue = this->getOutput(outputID, count-1);
         if (pastInputValue->status == Object::valid)
         {
            can_look_back=true;
            past = &object_cast<Vector<float> > (pastInputValue);
         }      
      }      
      
      //int size = filter.size();
      int delay = floor(.5+filter[1]);
      //filter[0]=1;

      for (int i=0;i<outputLength;i++)
         output[i]=in[i];

      if (can_look_back)
      {
	 for (int i=0;i<delay;i++)
	    //output[i] -= (*past)[length+i-delay];
	    output[i] += filter[0]*(*past)[length+i-delay];
      }
      
      
      for (int i=delay;i<outputLength;i++)
	 //output[i] -= in[i-delay];
	 output[i] += filter[0]*output[i-delay];
            

      output.status = Object::valid;
   }

};
