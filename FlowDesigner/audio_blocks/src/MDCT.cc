// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>
#include "window.h"
#include "mdct.h"

using namespace std;

class MDCT;

DECLARE_NODE(MDCT)
/*Node
 *
 * @name MDCT
 * @category DSP:TimeFreq
 * @require MDCT
 * @description MDCT implementation (taken from Vorbis)
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input frame
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description MDCT result
 *
 * @parameter_name LENGTH
 * @parameter_type int
 * @parameter_description Frame (not window) size
 *
END*/


class MDCT : public BufferedNode {
   
   int inputID;
   int outputID;
   int length;
   vector<float> buffer;
   vector<double> pcm;
   double *window;
   mdct_lookup m_look;

public:
   MDCT(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   , window(NULL)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      length = dereference_cast<int> (parameters.get("LENGTH"));

      buffer.resize(length*2);
      pcm.resize(length*2);
      for (int i=0;i<length*2;i++)
	 buffer[i]=0;
      inOrder = true;
   }

   ~MDCT() {if (window) free(window);}

   virtual void initialize()
   {
      window=_vorbis_window(0,length*2,length,length);
      mdct_init(&m_look,length*2);
      this->BufferedNode::initialize();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      
      out[count] = Vector<float>::alloc(length);
      Vector<float> &output = object_cast<Vector<float> > (out[count]);

      for (int i=0;i<length;i++)
	 buffer[i+length] = in[i];

      for (int i=0;i<length*2;i++)
	 pcm[i]=buffer[i]*window[i];

      mdct_forward(&m_look,&pcm[0],&pcm[0]);

      for (int i=0;i<length;i++)
      {
         output[i]=pcm[i];
      }

      for (int i=0;i<length;i++)
	 buffer[i] = buffer[i+length];
   }

};
