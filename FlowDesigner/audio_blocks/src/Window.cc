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

class Window;

//DECLARE_NODE(Window)
NODE_INFO(Window, "Signal:DSP", "INPUT", "OUTPUT", "LENGTH:WINDOW")

class Window : public FrameOperation {
   
   int inputID;
   int inputLength;
   vector<float> window;

public:
   Window(string nodeName, ParameterSet params)
      : FrameOperation(nodeName, params)
      , window(outputLength)

   {
      inputID = addInput("INPUT");
      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));
   }

   ~Window() {}

   virtual void specificInitialize()
   {
      this->FrameOperation::specificInitialize();
      int i;
      String type = object_cast<String> (parameters.get("WINDOW"));
      
      if (type == "HANNING")
      {
         for (i=0;i<inputLength;i++)
            window[i]=.5-.5*cos((2*M_PI*i)/inputLength);
      } else if (type == "HAMMING")
      {
         for (i=0;i<inputLength;i++)
            window[i]=.54-.46*cos((2*M_PI*i)/inputLength);
      } else 
      {
         throw new GeneralException("Unknown window type",__FILE__,__LINE__);
      }
      
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
      
      int i;
      int size=window.size();
      for (i=0;i<size;i++)
         output[i]=in[i]*window[i];
      
      output.status = Object::valid;
   }

};
