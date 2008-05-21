// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>
#include <speex/speex_echo.h>
#include <speex/speex_preprocess.h>

using namespace std;

namespace FD {

class AEC;

DECLARE_NODE(AEC);
 
/*Node
 *
 * @name AEC
 * @category DSP:Filter
 * @description Acoustic echo canceller (AEC) with non-linear processing (NLP)
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description The input audio
 *
 * @input_name FAR_END
 * @input_type Vector<float>
 * @input_description The far end audio
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Result of echo cancellation
 *
 * @parameter_name FRAME_SIZE
 * @parameter_type int
 * @parameter_value 128
 * @parameter_description Black size used
 *
 * @parameter_name TAIL_LENGTH
 * @parameter_type int
 * @parameter_value 1024
 * @parameter_description Filter length (tail), how long the echo can be
 *
 * @parameter_name SAMPLING_RATE
 * @parameter_type int
 * @parameter_value 8000
 * @parameter_description Sampling rate
 *
 * @parameter_name NLP
 * @parameter_type bool
 * @parameter_value true
 * @parameter_description Whether noise suppression (aka nonlinear processing) is used
 *
 * @parameter_name NOISE_SUPPRESS
 * @parameter_type int
 * @parameter_value 15
 * @parameter_description Amount of noise to suppress (dB)
 *
 * @parameter_name ECHO_SUPPRESS
 * @parameter_type int
 * @parameter_value 45
 * @parameter_description Amount of echo to suppress (dB)
 *
 * @parameter_name ECHO_SUPPRESS_ACTIVE
 * @parameter_type int
 * @parameter_value 10
 * @parameter_description Amount of noise to suppress when there is active speech (dB)
 *
END*/


class AEC : public BufferedNode {
   
   int inputID;
   int farEndID;
   int outputID;
   SpeexEchoState *state;
   SpeexPreprocessState *pre;
   int frameSize;
   spx_int32_t rate;
   int tail;
   bool nlp;
   
public:
   AEC(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      farEndID = addInput("FAR_END");
      outputID = addOutput("OUTPUT");
      
      frameSize = dereference_cast<int> (parameters.get("FRAME_SIZE"));
      tail = dereference_cast<int> (parameters.get("TAIL_LENGTH"));
      rate = dereference_cast<int> (parameters.get("SAMPLING_RATE"));
      nlp = dereference_cast<bool> (parameters.get("NLP"));
      state = speex_echo_state_init(frameSize, tail);
      speex_echo_ctl(state, SPEEX_ECHO_SET_SAMPLING_RATE, &rate);
      if (dereference_cast<bool> (parameters.get("NLP")))
      {
         pre = speex_preprocess_state_init(frameSize, rate);
         speex_preprocess_ctl(pre, SPEEX_PREPROCESS_SET_ECHO_STATE, state);
         int tmp = dereference_cast<int> (parameters.get("NOISE_SUPPRESS"));
         speex_preprocess_ctl(pre, SPEEX_PREPROCESS_SET_NOISE_SUPPRESS, &tmp);
         tmp = dereference_cast<int> (parameters.get("ECHO_SUPPRESS"));
         speex_preprocess_ctl(pre, SPEEX_PREPROCESS_SET_ECHO_SUPPRESS, &tmp);
         tmp = dereference_cast<int> (parameters.get("ECHO_SUPPRESS_ACTIVE"));
         speex_preprocess_ctl(pre, SPEEX_PREPROCESS_SET_ECHO_SUPPRESS_ACTIVE, &tmp);
      } else {
         pre = NULL;
      }
      inOrder = true;
   }
   
   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);
      ObjectRef farEndValue = getInput(farEndID, count);
      
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();
      const Vector<float> &far = object_cast<Vector<float> > (farEndValue);
      
      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;
      
      vector<spx_int16_t> ins(inputLength), fars(inputLength), outs(inputLength);
      for (int i=0;i<inputLength;i++)
         ins[i] = in[i];
      for (int i=0;i<inputLength;i++)
         fars[i] = far[i];
      //std:cerr << "cancel " << inputLength << std::endl;
      speex_echo_cancellation(state, &ins[0], &fars[0], &outs[0]);
      //speex_echo_capture(state, &ins[0], &outs[0], NULL);
      //speex_echo_playback(state, &fars[0]);
      speex_preprocess_run(pre, &outs[0]);
      for (int i=0;i<inputLength;i++)
         output[i] = outs[i];
   }
   
   IN_ORDER_NODE_SPEEDUP(AEC)
};
}//namespace FD
