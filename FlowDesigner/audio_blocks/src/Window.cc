// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>

class Window;

DECLARE_NODE(Window)
/*Node
 *
 * @name Window
 * @category DSP:Base
 * @description Applies a window on a frame
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input frame
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Windowed frame
 *
 * @parameter_name LENGTH
 * @parameter_type int
 * @parameter_description Length of the frames/window
 *
 * @parameter_name WINDOW
 * @parameter_type string
 * @parameter_description Window type (HANNING, HAMMING, HALF_HANNING)
 *
 * @parameter_name SYMETRIC
 * @parameter_type bool
 * @parameter_description Symetric window, uses (length-1) for normalization
 *
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
      
      int len = length;
      if (parameters.exist("SYMETRIC"))
      {
	 if (dereference_cast<bool> (parameters.get("SYMETRIC")))
	    len--;
      } else {
	 len--;
      }

      if (type == "HANNING")
      {
         for (i=0;i<length;i++)
            window[i]=.5-.5*cos((2*M_PI*i)/len);
      } else if (type == "HAMMING")
      {
         for (i=0;i<length;i++)
            window[i]=.54-.46*cos((2*M_PI*i)/len);
      } else if (type == "HALF_HANNING")
      {
         for (i=0;i<length;i++)
            window[i]=.5+.5*cos((M_PI*i)/len);
      } else 
      {
         throw new GeneralException("Unknown window type",__FILE__,__LINE__);
      }
      
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      
      if (in.size() != length)
      {
	 cerr << in.size() << " != " << length << endl;
	 throw new NodeException(this, "Size of input != size of window", __FILE__, __LINE__);
      }
      Vector<float> &output = *Vector<float>::alloc(length);
      out[count] = &output;
      
      for (int i=0;i<length;i++)
         output[i]=in[i]*window[i];
   }

NO_ORDER_NODE_SPEEDUP(Window)
};
