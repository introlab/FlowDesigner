// Copyright (C) 2001 LocusDialog
// Author: Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <stdlib.h>
#include <math.h>
#include "misc.h"

using namespace std;
using namespace FD;

class Noise;

DECLARE_NODE(Noise)
/*Node
 *
 * @name Noise
 * @category DSP:Misc
 * @description Noise generator
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Noise signal (uncorrelated)
 *
 * @parameter_name LENGTH
 * @parameter_type int
 * @parameter_description Length of the generated noise signal (frame length)
 *
 * @parameter_name TYPE
 * @parameter_type string
 * @parameter_description Noise type (UNIFORM, GAUSS, TRIANGLE)
 * @parameter_value GAUSS
 *
 * @parameter_name SD
 * @parameter_type float
 * @parameter_description Noise standard deviation
 *
END*/


class Noise : public BufferedNode {
   
   int inputID;
   int outputID;
   float sd;
   enum NoiseType {uniform, triangle, gauss};
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
      else
	 sd = 1;
      if (parameters.exist("TYPE"))
      {
	 if (object_cast<String> (parameters.get("TYPE")) == "UNIFORM")
	    type = uniform;
	 else if (object_cast<String> (parameters.get("TYPE")) == "GAUSS")
	    type = gauss;
	 else if (object_cast<String> (parameters.get("TYPE")) == "TRIANGLE")
	    type = triangle;
	 else
	    new NodeException(NULL, "Unknown function type", __FILE__, __LINE__);
      } else type = gauss;

      scale = sd*2.0*sqrt(3.0);
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      Vector<float> &output = *Vector<float>::alloc(length);
      out[count] = &output;

      if (type == uniform)
      {
	 for (int i=0;i<length;i++)
	 {
	    output[i] = scale*((float(rand())/float(RAND_MAX))-.5);
	 }
      } else if (type == gauss)
      {
	 for (int i=0;i<length;i++)
	    output[i]=gauss_rand(sd);
/*	 for (int i=0;i<(length+1)>>1;i++)
	 {
	    float U1, U2, S;
	    do {
	       U1 = float(rand())/float(RAND_MAX);
	       U2 = float(rand())/float(RAND_MAX);
	       U1 = 2*U1-1;
	       U2 = 2*U2-1;
	       S = U1*U1 + U2*U2;
	    } while (S>= 1);
	    output[i]          = sd*sqrt(-2 * log(S) / S) * U1;
	    output[length-i-1] = sd*sqrt(-2 * log(S) / S) * U2;
	 }
*/
      } else 
	 throw new NodeException(this, "Unknown function type", __FILE__, __LINE__);
   }

      
};
