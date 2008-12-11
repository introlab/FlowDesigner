// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <stdlib.h>
#include <math.h>
#include "macros_math.h"

using namespace std;

namespace FD {

class MLDetector;

DECLARE_NODE(MLDetector)
/*Node
 *
 * @name MLDetector
 * @category Communications:Receiver
 * @description Perform symbol-by-symbol maximum-likelihood detection based on symbol LLRs.
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input LLR sequence.
 *
 * @output_name OUTPUT
 * @output_type Vector<int>
 * @output_description Detected bits.
 *
 * @parameter_name NSIGNALS
 * @parameter_type int
 * @parameter_description Number of signals of constellation.
 * @parameter_value 2
 *
END*/


class MLDetector : public BufferedNode {
   
	int inputID;
	int outputID;
	int nsignals;
	int nbits;

public:
MLDetector(string nodeName, ParameterSet params)
: BufferedNode(nodeName, params)
{
	inputID = addInput("INPUT");
	outputID = addOutput("OUTPUT");

	if(parameters.exist("NSIGNALS"))
		nsignals = dereference_cast<int> (parameters.get("NSIGNALS"));
	else
		nsignals = 2;

	nbits = floor(log2(nsignals));
	if((1 << nbits) != nsignals)
		throw new NodeException(this, "Parameter nsignals is not a power fo 2", __FILE__, __LINE__);


}



void calculate(int output_id, int count, Buffer &out)
{

	int i, j;
	
	ObjectRef inputRef = getInput(inputID, count);
	const Vector<float> &inputVec = object_cast<Vector<float> >(inputRef);
	int length = inputVec.size();

	if(length % nsignals)
		throw new NodeException(this, "Length of input vector is not a multiple of nsignals", __FILE__, __LINE__);

	Vector<int> &output = *Vector<int>::alloc(nbits * length / nsignals);

	out[count] = &output;

	float max;
	int maxidx;
	for (i = 0; i < length / nsignals; i++)
	{
		max = -1.e9;
		maxidx = 0;
		for(j = 0; j < nsignals; j++)
		{
			if(inputVec[i * nsignals + j] > max)
			{
				max = inputVec[i * nsignals + j];
				maxidx = j;
			}
		}
		for(j = 0; j < nbits; j++)	// TODO: add nonbinary output
		{
			output[i * nbits + j] = (maxidx >> j) & 1;
		}
	}

}
};

}//namespace FD
