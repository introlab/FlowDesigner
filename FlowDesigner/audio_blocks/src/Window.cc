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

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>

class Window;

DECLARE_NODE(Window)
/*Node

 * @name Window
 * @category Signal:DSP
 * @description No description available

 * @input_name INPUT
 * @input_description No description available

 * @output_name OUTPUT
 * @output_description No description available

 * @parameter_name LENGTH
 * @parameter_description No description available

 * @parameter_name WINDOW
 * @parameter_description No description available

END*/


class Window : public BufferedNode {
   
   int inputID;
   int outputID;
   int length;
   vector<float> window;

public:
   Window(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)

   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      length = dereference_cast<int> (parameters.get("LENGTH"));
   }

   virtual void specificInitialize()
   {
      this->BufferedNode::specificInitialize();
      int i;
      window.resize(length);
      String type = object_cast<String> (parameters.get("WINDOW"));
      
      if (type == "HANNING")
      {
         for (i=0;i<length;i++)
            window[i]=.5-.5*cos((2*M_PI*i)/length);
      } else if (type == "HAMMING")
      {
         for (i=0;i<length;i++)
            window[i]=.54-.46*cos((2*M_PI*i)/length);
      } else 
      {
         throw new GeneralException("Unknown window type",__FILE__,__LINE__);
      }
      
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
      
      if (in.size() != length)
	 throw new NodeException(this, "Size of input != size of window", __FILE__, __LINE__);

      Vector<float> &output = *Vector<float>::alloc(length);
      out[count] = &output;
      
      for (int i=0;i<length;i++)
         output[i]=in[i]*window[i];
   }

};
