// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <stdlib.h>
#include <math.h>

class LTF;

DECLARE_NODE(LTF)
/*Node
 *
 * @name LTF
 * @category DSP:Filter
 * @description Long-term (comb) filter
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @input_name FILTER
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
END*/


class LTF : public BufferedNode {
   
   int inputID;
   int filterID;
   int outputID;
   int noncausal;
   bool continuous;

public:
   LTF(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      filterID = addInput("FILTER");
      outputID = addOutput("OUTPUT");
      inputsCache[inputID].lookBack=1;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);
      ObjectRef filterValue = getOutput(filterID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int length = in.size();
      const Vector<float> &filter = object_cast<Vector<float> > (filterValue);
      const Vector<float> *past;
      bool can_look_back = false;

      out[count] = Vector<float>::alloc(length);
      Vector<float> &output = object_cast<Vector<float> > (out[count]);
      if (count > 0)
      {
         ObjectRef pastInputValue = getInput(inputID, count-1);
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
	    output[i] -= filter[0]*(*past)[length+i-delay];
      }
      
      
      for (int i=delay;i<length;i++)
	 //output[i] -= in[i-delay];
	 output[i] -= filter[0]*in[i-delay];
            
   }

};
