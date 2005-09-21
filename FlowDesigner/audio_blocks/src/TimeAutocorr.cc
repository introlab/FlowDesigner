// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "vec.h"

using namespace std;

namespace FD {

class TimeAutocorr;

DECLARE_NODE(TimeAutocorr)
/*Node
 *
 * @name TimeAutocorr
 * @category DSP:Misc
 * @description Autocorrelation across vectors (frames)
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input vectors (frames)
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Autocorrelations (summed) for each delay
 *
 * @parameter_name INPUTLENGTH
 * @parameter_type int
 * @parameter_description Length ov input vectors
 *
 * @parameter_name LOOKAHEAD
 * @parameter_type int
 * @parameter_description Maximum forward (non-causal) delay
 *
 * @parameter_name LOOKBACK
 * @parameter_type int
 * @parameter_description Maximum backward (causal) delay
 *
END*/


class TimeAutocorr : public BufferedNode {
   
   int inputID;
   int outputID;

   int inputLength;
   int outputLength;

   int numberFrames;
   vector<Vector<float> *> frames;

public:
   TimeAutocorr(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)

   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");

      inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      
      inputsCache[inputID].lookAhead = dereference_cast<int> (parameters.get("LOOKAHEAD"));
      inputsCache[inputID].lookBack = dereference_cast<int> (parameters.get("LOOKBACK"));

      numberFrames=inputsCache[inputID].lookBack+inputsCache[inputID].lookAhead+1;
      frames.resize(numberFrames);
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      NodeInput input = inputs[inputID];

      Vector<float> &output = *Vector<float>::alloc(numberFrames);
      out[count] = &output;

      int i,j;
      if (count < inputsCache[inputID].lookBack)
      {
	 for (i=0;i<output.size();i++)
	    output[i]=0;
	 return;
      }


      vector <RCPtr<Vector<float> > > inVect;
      for (i = -inputsCache[inputID].lookBack, j=0; i <= inputsCache[inputID].lookAhead ; i++, j++)
      {
         ObjectRef inputValue = input.node->getOutput(input.outputID, count + i);

	 
	 inVect.insert(inVect.end(),RCPtr<Vector<float> > (inputValue));
      }      
      
      for (int i=0;i<output.size();i++)
      {
	 output[i] = vec_inner_prod(&(*inVect[0])[0], &(*inVect[i])[0], inputLength);
      }
   }

NO_ORDER_NODE_SPEEDUP(TimeAutocorr)
};

}//namespace FD
