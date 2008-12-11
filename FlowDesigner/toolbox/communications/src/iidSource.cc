// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "MersenneTwister.h"

#include <stdlib.h>
#include <math.h>

using namespace std;

namespace FD {

class iidSource;

DECLARE_NODE(iidSource)
/*Node
 *
 * @name iidSource
 * @category Communications:Sources
 * @description iid random sequence generator
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Generated random process
 *
 * @parameter_name LENGTH
 * @parameter_type int
 * @parameter_description Number of generated samples
 * @parameter_value 1000
 *
 * @parameter_name TYPE
 * @parameter_type string
 * @parameter_description Generated pdf (UNIFORM, GAUSSIAN)
 * @parameter_value GAUSSIAN
 *
 * @parameter_name MEAN
 * @parameter_type float
 * @parameter_description Mean
 * @parameter_value 0.
 *
 * @parameter_name VAR
 * @parameter_type float
 * @parameter_description Variance
 * @parameter_value 1.
 *
 * @parameter_name SEED
 * @parameter_type int
 * @parameter_description Generator seed (negative integer)
 * @parameter_value -1
END*/


class iidSource : public BufferedNode {
   
	int outputID;
	float mean, var;
	enum pdfType {UNIFORM, GAUSSIAN};
	pdfType type;
	int length;
	long int seed;
	MTRand rng;

public:
   iidSource(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");

      length = dereference_cast<int> (parameters.get("LENGTH"));

      if (parameters.exist("MEAN"))
	 mean = dereference_cast<float> (parameters.get("MEAN"));
      else
	 mean = 0.;

      if (parameters.exist("VAR"))
	 var = dereference_cast<float> (parameters.get("VAR"));
      else
	 var = 1.;

      if (parameters.exist("SEED"))
	 seed = dereference_cast<int> (parameters.get("SEED"));
      else
	 seed = -1;
      rng.seed(seed);

      if (parameters.exist("TYPE"))
      {
	 if (object_cast<String> (parameters.get("TYPE")) == "UNIFORM")
	    type = UNIFORM;
	 else if (object_cast<String> (parameters.get("TYPE")) == "GAUSSIAN")
	    type = GAUSSIAN;
	 else
	    throw new NodeException(NULL, "Unknown function type", __FILE__, __LINE__);
      } else type = UNIFORM;
   }

void calculate(int output_id, int count, Buffer &out)
{
	int i;
	Vector<float> &output = *Vector<float>::alloc(length);
	out[count] = &output;
	
	if(type == UNIFORM)
	{
		for (i = 0; i < length; i++)
		{
			output[i] = (rng() - 0.5) * sqrt(12. * var) + mean;
		}
	}
	else if(type == GAUSSIAN)
	{
		for(i = 0; i < length; i++)
		{
			output[i] = sqrt(var) * normal() + mean;
		}
	}
	else
		throw new NodeException(this, "Unknown function type", __FILE__, __LINE__);
}



float normal()
{

	float u1, u2, mag, inv;
	static float g2;
	static int rdy = 0;

	if(rdy)
	{
		rdy = 0;
		return g2;
	}
	else
	{
		do
		{
			u1 = 2. * rng() - 1.;
			u2 = 2. * rng() - 1.;
			mag = u1 * u1 + u2 * u2;
		}while((mag >= 1.) || (mag == 0.));

		inv = sqrt(-2. * log(mag) / mag);

		g2 = u1 * inv;
		rdy = 1;
		return u2 * inv;
	}
}

};	// class iidSource

}	// namespace FD
