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

class ComplexGaussian;

DECLARE_NODE(ComplexGaussian)
/*Node
 *
 * @name ComplexGaussian
 * @category Communications:Sources
 * @description iid complex Gaussian random sequence generator
 *
 * @output_name OUTPUT
 * @output_type Vector<complex<float> >
 * @output_description Generated random process
 *
 * @parameter_name LENGTH
 * @parameter_type int
 * @parameter_description Number of generated samples
 * @parameter_value 1000
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
 *
END*/


class ComplexGaussian : public BufferedNode {
   
	int outputID;
	float mean, var;
	int length;
	long int seed;
	MTRand rng;

public:
   ComplexGaussian(string nodeName, ParameterSet params)
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

}





void calculate(int output_id, int count, Buffer &out)
{
	int i;
	Vector<complex<float> > &output = *Vector<complex<float> >::alloc(length);
	out[count] = &output;

	for(i = 0; i < length; i++)
	{
		output[i] = sqrt(var) * normal() + mean;
	}
}





complex<float> normal()
{

	float u1, u2, mag, inv;

	do
	{
		u1 = 2. * rng() - 1.;
		u2 = 2. * rng() - 1.;
		mag = u1 * u1 + u2 * u2;
	}while((mag >= 1.) || (mag == 0.));

	inv = sqrt(-2. * log(mag) / mag);

	return complex<float>(u1 * inv, u2 * inv);
} 



};	// class ComplexGaussian

}	// namespace FD
