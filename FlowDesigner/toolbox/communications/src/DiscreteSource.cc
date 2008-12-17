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

class DiscreteSource;

DECLARE_NODE(DiscreteSource)
/*Node
 *
 * @name DiscreteSource
 * @category Communications:Sources
 * @description iid random sequence generator
 *
 * @output_name OUTPUT
 * @output_type Vector<int>
 * @output_description Generated suorce sequence.
 *
 * @parameter_name LENGTH
 * @parameter_type int
 * @parameter_description Number of generated symbols.
 * @parameter_value 1000
 *
 * @parameter_name ALPHABETSIZE
 * @parameter_type int
 * @parameter_description Number of symbols in the source alphabet.
 * @parameter_value 2
 *
 * @parameter_name SEED
 * @parameter_type int
 * @parameter_description RNG seed.
 * @parameter_value -1

END*/


class DiscreteSource : public BufferedNode	// TODO: add nonuniform distribution
{
   
	int outputID;
	int length;
	int asize;
	long int seed;
	MTRand rng;

public:
   DiscreteSource(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");

      length = dereference_cast<int> (parameters.get("LENGTH"));

      if (parameters.exist("ALPHABETSIZE"))
	 asize = dereference_cast<int> (parameters.get("ALPHABETSIZE"));
      else
	 asize = 2;

      if (parameters.exist("SEED"))
	 seed = dereference_cast<int> (parameters.get("SEED"));
      else
	 seed = -1;

      rng.seed(seed);

   }

void calculate(int output_id, int count, Buffer &out)
{
	int i;
	Vector<int> &output = *Vector<int>::alloc(length);
	out[count] = &output;
	for (i = 0; i < length; i++)
	{
		output[i] = floor(rng() * asize);
	}
}


};	// class DiscreteSource

}	// namespace FD
