// Copyright (C) 1999 Jean-Marc Valin
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include <stream.h>
#include "FrameOperation.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>
#include "window.h"
#include "mdct.h"
//#include "codec.h"
//#include "psy.h"

class MDCT;



DECLARE_NODE(MDCT)
/*Node

 * @name MDCT
 * @category Signal:DSP
 * @description No description available

 * @input_name INPUT
 * @input_description No description available

 * @output_name OUTPUT
 * @output_description No description available

 * @parameter_name LENGTH
 * @parameter_description No description available

END*/


class MDCT : public FrameOperation {
   
   int inputID;
   int inputLength;
   vector<float> buffer;
   vector<double> pcm;
   double *window;
   mdct_lookup m_look;
      //vorbis_look_psy p_look;

public:
   MDCT(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      inputID = addInput("INPUT");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));

      buffer.resize(inputLength*2);
      pcm.resize(inputLength*2);
      for (int i=0;i<inputLength*2;i++)
	 buffer[i]=0;
      inOrder = true;
   }

   ~MDCT() {free(window);}

   virtual void specificInitialize()
   {
      window=_vorbis_window(0,inputLength*2,inputLength,inputLength);
      mdct_init(&m_look,inputLength*2);
      //_vp_psy_init(&p_look,&_psy_set0,inputLength,16000);
      
      this->FrameOperation::specificInitialize();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      NodeInput input = inputs[inputID];
      ObjectRef inputValue = input.node->getOutput(input.outputID, count);

      Vector<float> &output = object_cast<Vector<float> > (out[count]);
      if (inputValue->status != Object::valid)
      {
         output.status = inputValue->status;
         return;
      }
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      
      for (int i=0;i<inputLength;i++)
	 buffer[i+inputLength] = in[i];

      for (int i=0;i<inputLength*2;i++)
	 pcm[i]=buffer[i]*window[i];

      mdct_forward(&m_look,pcm.begin(),pcm.begin());

      for (int i=0;i<outputLength;i++)
      {
         output[i]=pcm[i];
      }

      for (int i=0;i<inputLength;i++)
	 buffer[i] = buffer[i+inputLength];
      
      output.status = Object::valid;
   }

};
