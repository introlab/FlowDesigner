// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#include "Node.h"
#include "Vector.h"

using namespace std;

namespace FD {

class AveragePower;

DECLARE_NODE(AveragePower)
/*Node
 *
 * @name AveragePower
 * @category Communications:Statistics
 * @description Estimate the bit error rate (BER)
 *
 * @input_name INPUT
 * @input_type Vector<complex<float> >
 * @input_description Input signal.
 *
 * @output_name OUTPUT
 * @output_type float
 * @output_description Estimated power.
 *
 * @parameter_name MEMORY
 * @parameter_type bool
 * @parameter_description Perform average across blocks
 * @parameter_value true
 
 *
END*/


class AveragePower : public Node {
   
	int inputID;
	int outputID;
	
	double APow;
	unsigned long int nsamples;
	bool memory;

public:
AveragePower(string nodeName, ParameterSet params)
: Node(nodeName, params)
{
	inputID = addInput("INPUT");
	outputID = addOutput("OUTPUT");

	if (params.exist("MEMORY"))
		memory = dereference_cast<bool>(params.get("MEMORY"));
	else memory = true;

	APow = 0.;
	nsamples = 0;
}


void reset()
{
	Node::reset();
	APow = 0.;
	nsamples = 0;
}


ObjectRef getOutput(int output_id, int count)
{
	unsigned int i;
	unsigned int length;
	double tpow = 0.;
	
	ObjectRef inputRef = getInput(inputID, count);
	const Vector<complex<float> > &inputVec = object_cast<Vector<complex<float> > >(inputRef);
	length = inputVec.size();

	if(!memory) reset();

	for(i = 0; i < length; i++)
	{
		tpow += inputVec[i].real() * inputVec[i].real();
		tpow += inputVec[i].imag() * inputVec[i].imag();
	}

	APow = APow * (nsamples / float(nsamples + length)) + tpow / float(nsamples + length);
	nsamples += length;

	return ObjectRef(Float::alloc(float(APow)));
}

};	// class AveragePower

}	// namespace FD
