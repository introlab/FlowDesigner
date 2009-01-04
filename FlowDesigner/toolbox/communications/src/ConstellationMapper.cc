// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#include "ConstellationMapper.h"

using namespace std;

namespace FD {

DECLARE_NODE(ConstellationMapper)

/*Node
 *
 * @name ConstellationMapper
 * @category Communications:Transmitter
 * @require Constellation
 * @description Generate a sequence of constellation points
 *
 * @input_name INPUT
 * @input_type Vector<int>
 * @input_description Binary input sequence.
 *
 * @output_name OUTPUT
 * @output_type Vector<complex<float> >
 * @output_description Generated sequence.
 *
 * @parameter_name TYPE
 * @parameter_type string
 * @parameter_description Modulation type (PAM, PSK, QAM, FILE).
 * @parameter_value PSK
 *
 * @parameter_name NBITS
 * @parameter_type int
 * @parameter_description Number of bits per symbol.
 * @parameter_value 1
 *
 * @parameter_name ENERGY
 * @parameter_type float
 * @parameter_description Average symbol energy per complex dimension.
 * @parameter_value 1.
 *
 * @parameter_name FILENAME
 * @parameter_type string
 * @parameter_description Name of file containing the constellation.
 * @parameter_value Xi.txt
END*/


ConstellationMapper::ConstellationMapper(string nodeName, ParameterSet params)
: BufferedNode(nodeName, params), Xii(params)
{
	inputID = addInput("INPUT");
	outputID = addOutput("OUTPUT");

	if (parameters.exist("NBITS"))
		nbits = dereference_cast<int> (parameters.get("NBITS"));
	else nbits = 1;

}


ConstellationMapper::~ConstellationMapper()
{
	// Xiptr->destroy();
}


void ConstellationMapper::calculate(int output_id, int count, Buffer &out)
{
	int i, j, k;
	
	ObjectRef inputRef = getInput(inputID, count);
	const Vector<int> &inputVec = object_cast<Vector<int> >(inputRef);
	int length = inputVec.size();

	if(length % nbits) throw new NodeException(this, "ConstellationMapper: input length % nbits != 0", __FILE__, __LINE__);

	Vector<complex<float> > &output = *Vector<complex<float> >::alloc(length / nbits);

	out[count] = &output;

	for (i = 0; i < length / nbits; i++)
	{
		k = 0;				// TODO: add nonbinary input
		for(j = 0; j < nbits; j++)
		{
			k += (inputVec[i * nbits + j] & 1) << j;
		}
		output[i] = Xii[k];
	}

}

}//namespace FD
