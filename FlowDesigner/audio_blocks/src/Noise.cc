// Copyright (C) 2001 LocusDialog
// Author: Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <stdlib.h>
#include <math.h>

class Noise;

DECLARE_NODE(Noise)
/*Node
 *
 * @name Noise
 * @category Signal:DSP
 * @description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name LENGTH
 * @parameter_description No description available
 *
 * @parameter_name TYPE
 * @parameter_description No description available
 *
 * @parameter_name SD
 * @parameter_description No description available
 *
END*/


class Noise : public BufferedNode {
   
   int inputID;
   int outputID;
   float sd;
   enum NoiseType {Square, Triangle, Gauss};
   NoiseType type;
   int length;
   float scale;

public:
   Noise(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      length = dereference_cast<int> (parameters.get("LENGTH"));
      if (parameters.exist("SD"))
	 sd = dereference_cast<float> (parameters.get("SD"));
      if (parameters.exist("TYPE"))
      {
	 if (dereference_cast<string> (parameters.get("TYPE")) == "UNIFORM")
	    type = Square;
      } else type = Square;

      scale = sd*2.0*sqrt(3);
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      Vector<float> &output = *Vector<float>::alloc(length);
      out[count] = &output;

      for (int i=0;i<length;i++)
      {
	 output[i] = scale*((float(rand())/float(RAND_MAX))-.5);
      }
      
   }

      
};
