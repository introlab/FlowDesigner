// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "misc.h"

#ifdef HAVE_VALUES_H
#include <values.h>
#endif

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

class ArgMax;

DECLARE_NODE(ArgMax)
/*Node
 *
 * @name ArgMax
 * @category DSP:Base
 * @description Finds the maximum value in a vector
 *
 * @input_name INPUT
 * @input_type Vector
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description Index 0 contains the maximum value, index 1 contains the index where the maximum is found
 *
 * @parameter_name START
 * @parameter_type int
 * @parameter_description Index where search is started
 *
 * @parameter_name END
 * @parameter_type int
 * @parameter_description Index where search ends
 *
END*/


class ArgMax : public BufferedNode {
   
   int inputID;
   int outputID;
   int start;
   int end;

public:
   ArgMax(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");

      start = dereference_cast<int> (parameters.get("START"));
      end = dereference_cast<int> (parameters.get("END"));

   }


   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
         return;
      }
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(2);
      out[count] = &output;

      int maxcep;
      float maxval;
      
      maxval=-FLT_MAX;
      maxcep=0;
      for (int i=start;i<=end;i++)         /*find the maximum cep (positive only)*/
      {
         if (in[i]>maxval) 
         {
            maxcep=i;
            maxval=in[i];
         }
      }      
      
      output[0]=maxval;
      output[1]=maxcep;
      
   }

};
