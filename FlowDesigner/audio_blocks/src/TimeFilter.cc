// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <strstream>

using namespace std;

namespace FD {

class TimeFilter;

DECLARE_NODE(TimeFilter)
/*Node
 *
 * @name TimeFilter
 * @category DSP:Filter
 * @description Filters across vectors (frames)
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input vectors (frames)
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Filtered vectors
 *
 * @parameter_name LENGTH
 * @parameter_type int
 * @parameter_description Vector length
 *
 * @parameter_name FIR
 * @parameter_type string
 * @parameter_description FIR part as <Vector<float> ... >
 *
 * @parameter_name IIR
 * @parameter_type string
 * @parameter_description IIR part as <Vector<float> ... >
 *
 * @parameter_name LOOKAHEAD
 * @parameter_type int
 * @parameter_description Non-causality (in frames)
 *
END*/


class TimeFilter : public BufferedNode {
   
   int inputID;
   int outputID;
   int inputLength;

   int lookAhead;
   vector<float> fir;
   vector<float> iir;
   //bool first_use;

public:
   TimeFilter(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)

   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));

      istrstream fir_str(object_cast <String> (parameters.get("FIR")).c_str());
      istrstream iir_str(object_cast <String> (parameters.get("IIR")).c_str());
      fir_str >> fir;
      iir_str >> iir;
      
      if (parameters.exist("LOOKAHEAD"))
         inputsCache[inputID].lookAhead = dereference_cast<int> (parameters.get("LOOKAHEAD"));
      inputsCache[inputID].lookBack = fir.size() - 1 - inputsCache[inputID].lookAhead;
      
   }

   virtual void initialize()
   {
      outputs[outputID].lookBack += iir.size() - 1;
      this->BufferedNode::initialize();
      
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      //Vector<float> &output = object_cast<Vector<float> > (out[count]);
      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;

      NodeInput input = inputs[inputID];

      int i,j;
      
      for (j=0;j<inputLength;j++)
         output[j] = 0.0;
      //int fir_limit = min(fir.size() - 1, count + inputsCache[inputID].lookAhead + 1 - fir.size());
      int fir_limit = fir.size() - 1;
      for (i = 0; i <= fir_limit ; i++)
      {
	 if (count - i + inputsCache[inputID].lookAhead < 0)
	    break;
         ObjectRef inputValue = input.node->getOutput(input.outputID, count - i + inputsCache[inputID].lookAhead);
         //cerr << "inputsCache[inputID].lookAhead = " << inputsCache[inputID].lookAhead << endl;

         const Vector<float> &firRow = object_cast<Vector<float> > (inputValue);
         for (j = 0; j < inputLength ; j++)
            output[j] += fir[i]*firRow[j];
      }
      //int iir_limit = min(iir.size() - 1, count + inputsCache[inputID].lookAhead + 1 - fir.size());
      int dummy=iir.size()-1;
      int iir_limit = min(dummy, count);
      //cerr << name << " " << iir_limit << endl;
      //cerr << count << " " << inputsCache[inputID].lookAhead << " " << 
      for (i = 1; i <= iir_limit ; i++)
      {
         ObjectRef inputValue = this->getOutput(outputID, count - i);
         const Vector<float> &iirRow = object_cast<Vector<float> > (inputValue);
         for (j = 0; j < inputLength ; j++)
            output[j] -= iir[i]*iirRow[j];
      }

   }

NO_ORDER_NODE_SPEEDUP(TimeFilter)
};

}//namespace FD
