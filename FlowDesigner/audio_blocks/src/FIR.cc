// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <stdlib.h>

class FIR;

DECLARE_NODE(FIR)
/*Node
 *
 * @name FIR
 * @category DSP:Filter
 * @description Finite Impulse Response (FIR) filter
 *
 * @input_name INPUT
 * @input_type Vector
 * @input_description Input frame
 *
 * @input_name FILTER
 * @input_type Vector
 * @input_description Filter coefficients
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description Filtered output
 *
 * @parameter_name CONTINUOUS
 * @parameter_type bool
 * @parameter_description Should the frames be considered continuous (filter with memory). Default is true
 *
 * @parameter_name NONCAUSAL
 * @parameter_type int
 * @parameter_description Non-causality in number of samples. Default is causal filter
 *
END*/


class FIR : public BufferedNode {
   
   int inputID;
   int outputID;
   int filterID;
   int noncausal;
   bool continuous;

public:
   FIR(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");

      filterID = addInput("FILTER");

      if (parameters.exist("CONTINUOUS"))
      {
	 ObjectRef cont = parameters.get("CONTINUOUS");
	 if (typeid(*cont) == typeid(Bool))
	    continuous = dereference_cast<bool> (cont);
	 else if (typeid(*cont) == typeid(Int))
	    continuous = dereference_cast<int> (cont);
	 else 
	    continuous = true;
      } else
         continuous=true;
      
      if (parameters.exist("NONCAUSAL"))
         noncausal = dereference_cast<int> (parameters.get("NONCAUSAL"));
      else 
         noncausal=0;

      if (continuous)
         inputsCache[inputID].lookBack=1;
      if (noncausal && continuous)
         inputsCache[inputID].lookAhead=1;
 
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
         output[i]=0;

      const Vector<float> &filter = object_cast<Vector<float> > (filterValue);

      const Vector<float> *past;
      const Vector<float> *next;
      bool can_look_back = false;
      bool can_look_ahead = false;
      if (count > 0 && continuous)   
      {
         ObjectRef pastInputValue = getInput(inputID, count-1);
	 can_look_back=true;
	 past = &object_cast<Vector<float> > (pastInputValue);
      }      
      
      if (noncausal && continuous)
      {
         ObjectRef nextInputValue = getInput(inputID, count+1);
	 can_look_ahead=true;
	 next = &object_cast<Vector<float> > (nextInputValue);
      }

      int size = filter.size();
      
      //past frames
      if (can_look_back)
      {
         for (int i=0;i<size-noncausal-1;i++)
         {
            int j, k;
            for (j=inputLength-1, k=1+i+noncausal; k<size ; j--, k++)
            {
               output[i]+=(*past)[j]*filter[k];
            }
         }
      }

      //future frames
      if (can_look_ahead)
      {
         for (int i=inputLength-noncausal;i<inputLength;i++)
         {
            int j, k;
            for (j=i+noncausal-inputLength, k=0; j>= 0 && k<size; j--, k++)
            {
               output[i]+=(*next)[j]*filter[k];
            }
         }
      }

      //current frames
      for (int i=0;i<inputLength;i++)
      {
         int j, k;
         j=min(i+noncausal,inputLength-1);
         k=i+noncausal-j;
         for (; j>=max(0,i+noncausal-size+1) ; j--, k++)
         {
            output[i]+=in[j]*filter[k];
         }
      }
            
   }

};
