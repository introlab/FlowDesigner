// Copyright (C) 1999 Jean-Marc Valin

#include "FrameOperation.h"
#include "Buffer.h"
#include "Vector.h"
#include <stdlib.h>
#include <math.h>

class ILTF;

DECLARE_NODE(ILTF)
/*Node

 * @name ILTF
 * @category Signal:DSP
 * @description No description available

 * @input_name INPUT
 * @input_description No description available

 * @input_name FILTER
 * @input_description No description available

 * @output_name OUTPUT
 * @output_description No description available

 * @parameter_name LENGTH
 * @parameter_description No description available

END*/


class ILTF : public FrameOperation {
   
   int inputID;
   int inputLength;
   int filterID;
   int noncausal;
   bool continuous;

public:
   ILTF(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      inputID = addInput("INPUT");
      filterID = addInput("FILTER");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));

      
 
   }

   ~ILTF() {}

   virtual void specificInitialize()
   {
      outputs[outputID].lookBack += 1;
      this->FrameOperation::specificInitialize();
      
      
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      NodeInput input = inputs[inputID];
      ObjectRef inputValue = input.node->getOutput(input.outputID, count);

      NodeInput filterInput = inputs[filterID];
      ObjectRef filterValue = filterInput.node->getOutput(filterInput.outputID, count);

      Vector<float> &output = object_cast<Vector<float> > (out[count]);
      if (inputValue->status != Object::valid)
      {
         output.status = inputValue->status;
         return;
      }
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      const Vector<float> &filter = object_cast<Vector<float> > (filterValue);
      const Vector<float> *past;
      bool can_look_back = false;

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
	    //output[i] -= (*past)[inputLength+i-delay];
	    output[i] += filter[0]*(*past)[inputLength+i-delay];
      }
      
      
      for (int i=delay;i<outputLength;i++)
	 //output[i] -= in[i-delay];
	 output[i] += filter[0]*output[i-delay];
            

      output.status = Object::valid;
   }

};
