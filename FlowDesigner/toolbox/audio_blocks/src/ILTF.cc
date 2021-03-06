// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <stdlib.h>
#include <math.h>

using namespace std;

namespace FD {

class ILTF;

DECLARE_NODE(ILTF)
/*Node
 *
 * @name ILTF
 * @category DSP:Filter
 * @description Inverse (all-pole) long-term (comb) filter
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input frame
 *
 * @input_name FILTER
 * @input_type Vector<float>
 * @input_description Filter params as [gain, period]
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
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
      int length = in.size();
      const Vector<float> &filter = object_cast<Vector<float> > (filterValue);
      const Vector<float> *past;
      bool can_look_back = false;

      out[count] = Vector<float>::alloc(length);
      Vector<float> &output = object_cast<Vector<float> > (out[count]);
      if (count > 0)   
      {
         ObjectRef pastInputValue = this->getOutput(outputID, count-1);
	 can_look_back=true;
	 past = &object_cast<Vector<float> > (pastInputValue);
      }      
      
      //int size = filter.size();
      int delay = int(floor(.5+filter[1]));
      //filter[0]=1;

      for (int i=0;i<length;i++)
         output[i]=in[i];

      if (can_look_back)
      {
	 for (int i=0;i<delay;i++)
	    //output[i] -= (*past)[length+i-delay];
	    output[i] += filter[0]*(*past)[length+i-delay];
      }
      
      
      for (int i=delay;i<length;i++)
	 //output[i] -= in[i-delay];
	 output[i] += filter[0]*output[i-delay];
            
   }

};
}//namespace FD
