// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <map>
#include "FFTWrap.h"

using namespace std;

namespace FD {

class FFT;

DECLARE_NODE(FFT)
/*Node
 *
 * @name FFT
 * @category DSP:TimeFreq
 * @require FFT
 * @description Computes the real FFT of a float vector
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description The input vector
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description The FFT resuls as [r(0), r(1),..., r(N/2), i(N/2-1), ..., i(2), i(1)]
 *
END*/


class FFT : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   FFT(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      RCPtr<Vector<float> > inputValue = getInput(inputID, count);

      const Vector<float> &in = *inputValue;      

      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;
      
      FFTWrap.rfft(&in[0], &output[0], inputLength);
   }

};
}//namespace FD
