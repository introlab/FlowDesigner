// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#include "Interleaver.h"

using namespace FD;


DECLARE_NODE(Interleaver)
/*Node
 *
 * @name Interleaver
 * @category Communications:Tools
 * @description Perform a pemutation of the input block.
 *
 * @input_name INPUT
 * @parameter_type int
 * @input_description Input block.
 *
 * @output_name OUTPUT
 * @parameter_type int
 * @output_description Permuted block.
 *
 * @parameter_name LENGTH
 * @parameter_type int
 * @parameter_description Block length.
 * @parameter_value 1
 *
 * @parameter_name INVERSE
 * @parameter_type bool
 * @parameter_description If true, inverse permutation is applied.
 * @parameter_value false
 *
END*/


Interleaver::Interleaver(string nodeName, ParameterSet params) : BufferedNode(nodeName, params)
{
	inputID = addInput("INPUT");
	outputID = addOutput("OUTPUT");

	if(parameters.exist("LENGTH"))
		length = dereference_cast<int> (parameters.get("LENGTH"));
	else
		length = 1;

	if(parameters.exist("INVERSE"))
		inverse = dereference_cast<bool>(parameters.get("INVERSE"));
	else
		inverse = false;

	perm.resize(length);
	for(int i = 0; i < length; i++) perm[i] = i;
}


void Interleaver::calculate(int output_id, int count, Buffer &out)
{
	ObjectRef inputRef = getInput(inputID, count);

	// TODO: implement this function in a type-independent way.

	const Vector<int> &inputVec = object_cast<Vector<int> >(inputRef);
	if(length != inputVec.size())
		throw new NodeException(this, "Length of input vector differs from interleaver size.", __FILE__, __LINE__);

	Vector<int> &output = *Vector<int>::alloc(length);

	out[count] = &output;

	if(inverse)
	{
		for(int i = 0; i < length; i++)
		{
			output[i] = inputVec[perm[i]];
		}
	}
	else
	{
		for(int i = 0; i < length; i++)
		{
			output[perm[i]] = inputVec[i];
		}
	}


}

