// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <algorithm>

using namespace std;
using namespace FD;

class TimeMedian;

DECLARE_NODE(TimeMedian)
/*Node
 *
 * @name TimeMedian
 * @category DSP:Filter
 * @description Performs median filtering across vectors (frames)
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input vectors (frames)
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Median-filtered vectors
 *
 * @parameter_name LENGTH
 * @parameter_type int
 * @parameter_description Vector size
 *
 * @parameter_name LOOKAHEAD
 * @parameter_type int
 * @parameter_description Median look back (number of frames)
 *
 * @parameter_name LOOKBACK
 * @parameter_type int
 * @parameter_description Median look ahead (number of frames)
 *
END*/


class TimeMedian : public BufferedNode {
   
   int inputID;
   int outputID;
   int length;

   int lookBack;
   int lookAhead;
   vector<vector<float> > data;

public:
   TimeMedian(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)

   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      length = dereference_cast<int> (parameters.get("LENGTH"));
      
      lookBack = dereference_cast<int> (parameters.get("LOOKBACK"));
      inputsCache[inputID].lookBack = lookBack;

      lookAhead = dereference_cast<int> (parameters.get("LOOKAHEAD"));
      inputsCache[inputID].lookAhead = lookAhead;
      
      data.resize(length);
      for (int i = 0;i<length;i++)
	 data[i].resize(lookAhead+lookBack+1);
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      Vector<float> &output = *Vector<float>::alloc(length);
      out[count] = &output;

      NodeInput input = inputs[inputID];

      int i,j;
      
      for (j=0;j<length;j++)
         output[j] = 0.0;

      int nbValid=0;

      for (i = -lookBack; i <= lookAhead ; i++)
      {
	 if (count + i < 0)
	    continue;

         ObjectRef inputValue = input.node->getOutput(input.outputID, count + i);

	 const Vector<float> &in = object_cast<Vector<float> > (inputValue);

	 for (int i=0;i<length;i++)
	    data[i][nbValid] = in[i];
	 nbValid++;

      }

      for (int i=0;i<length;i++)
      {
	 sort(&(data[i][0]),&(data[i][0])+nbValid);
	 if (nbValid&1)
	    output[i] = data[i][(nbValid-1)>>1];
	 else
	    output[i] = .5*(data[i][(nbValid>>1)-1]+data[i][nbValid>>1]);
      }

   }
NO_ORDER_NODE_SPEEDUP(TimeMedian)

};
