// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#include "CyclicEncoder.h"

using namespace std;

namespace FD {

DECLARE_NODE(CyclicEncoder)

/*Node
 *
 * @name CyclicEncoder
 * @category Communications:Transmitter
 * @description Systematic binary cyclic encoder
 *
 * @input_name INPUT
 * @input_type Vector<int>
 * @input_description Binary input sequence
 *
 * @output_name OUTPUT
 * @output_type Vector<int>
 * @output_description Binary output sequence
 *
 * @parameter_name GENERATOR
 * @parameter_type string
 * @parameter_description Generator polynomial in octal
 * @parameter_value 6
 *
 * @parameter_name K
 * @parameter_type int
 * @parameter_description Number of information bits
 * @parameter_value 255
 *
END*/


CyclicEncoder::CyclicEncoder(string nodeName, ParameterSet params)
: BufferedNode(nodeName, params)
{
	inputID = addInput("INPUT");
	outputID = addOutput("OUTPUT");

	if(params.exist("K"))
		K = dereference_cast<int>(params.get("K"));
	else K = 255;

	if(params.exist("GENERATOR"))
		g << object_cast<String>(params.get("GENERATOR"));
	else g << string("6");

	w << "0";
	x << "0";
}


CyclicEncoder::~CyclicEncoder()
{
	// Xiptr->destroy();
}


void CyclicEncoder::calculate(int output_id, int count, Buffer &out)
{
	int i;
	ObjectRef inputRef = getInput(inputID, count);
	const Vector<int> &inputVec = object_cast<Vector<int> >(inputRef);
	int length = inputVec.size();

	if(length != K) throw new NodeException(this,
			"CyclicEncoder: input length != K", __FILE__, __LINE__);

	Vector<int> &output = *Vector<int>::alloc(length + g.degree());

	out[count] = &output;

	w = inputVec;			// w(D) is the information word.

	w.upshift(g.degree());		// w(D) D^{n-k}
	x = w - w % g;			// x(D) is the code word.

	for(i = 0; i < output.size(); i++) output[i] = x[i];
}

}//namespace FD
