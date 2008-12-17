// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#include "Node.h"
#include "Vector.h"
#include "macros_math.h"

using namespace std;

namespace FD {

class MutualInf;

DECLARE_NODE(MutualInf)
/*Node
 *
 * @name MutualInf
 * @category Communications:Statistics
 * @description Estimate the mutual information.
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description LLR input sequence.
 *
 * @input_name INPUT_REF
 * @input_type Vector<int>
 * @input_description Reference discrete input sequence.
 *
 * @output_name MI
 * @output_type float
 * @output_description Estimated mutual information.
 *
 * @parameter_name MIN_ESTLENGTH
 * @parameter_type int
 * @parameter_description Minimum estimation length.
 * @parameter_value 1000
 *
 * @parameter_name NSYMBOLS
 * @parameter_type int
 * @parameter_description Number of symbols in the source alphabet.
 * @parameter_value 2
 *
 * @output_name NOT_FINISHED
 * @output_type bool
 * @output_description Termination condition.
END*/


class MutualInf : public Node {
   
	int inputID, inputIDref;
	int outputIDmi, outputIDcond;
	int min_estlength;
	int nsymbols;

	int estlength;
	double mi;
	
	int oflag;
	
	ObjectRef falseobj, trueobj;

public:
MutualInf(string nodeName, ParameterSet params)
: Node(nodeName, params)
{
	inputID = addInput("INPUT");
	inputIDref = addInput("INPUT_REF");
	outputIDmi = addOutput("MI");
	outputIDcond = addOutput("NOT_FINISHED");

	if(parameters.exist("NSYMBOLS"))
		nsymbols = dereference_cast<int> (parameters.get("NSYMBOLS"));
	else nsymbols = 2;

	if(parameters.exist("MIN_ESTLENGTH"))
		min_estlength = dereference_cast<int> (parameters.get("MIN_ESTLENGTH"));
	else min_estlength = 1000;

	oflag = 3;
	estlength = 0;
	mi = 0.;

	trueobj = ObjectRef(new Bool(true));
	falseobj = ObjectRef(new Bool(false));
}



ObjectRef getOutput(int output_id, int count)
{
	int i, j, k;
	int length;
	double m;
	int nbits = log2(nsymbols);
	
	ObjectRef inputRef = getInput(inputID, count);
	ObjectRef inputrefRef = getInput(inputIDref, count);
	const Vector<float> &inputVec = object_cast<Vector<float> >(inputRef);
	const Vector<int> &inputrefVec = object_cast<Vector<int> >(inputrefRef);

	if((oflag >= 3) && (estlength < min_estlength))
	{
		if(inputVec.size() % nsymbols)
		{
			throw new NodeException(this, "Input vector size not multiple of nsymbols.", __FILE__, __LINE__);
		}
		length = inputVec.size() / nsymbols;
		if(inputrefVec.size() / log2(nsymbols) != length)
		{
			throw new NodeException(this, "Reference symbol sequence and received symbol sequence have different lengths", __FILE__, __LINE__);
		}

		for(i = 0; i < length; i++)
		{
			m = inputVec[i * nsymbols];
			for(j = 1; j < nsymbols; j++)
			{
				m = MAXSTAR(m, inputVec[i * nsymbols + j]);
			}
		
			k = 0;				// TODO: add nonbinary input
			for(j = 0; j < nbits; j++)
			{
				k += (inputrefVec[i * nbits + j] & 1) << j;
			}

			mi += m - inputVec[i * nsymbols + k];
		}
		estlength += length;
		oflag = 0;
	}

	if(output_id == outputIDmi){
		oflag += 1;
		return ObjectRef(Float::alloc(log2(nsymbols) - mi / log(2.) / estlength));
	}
	else if(output_id == outputIDcond)
	{
		oflag += 2;
		if((estlength >= min_estlength) && ((oflag & 1) >= 1))
			return ObjectRef(falseobj);
		else
			return ObjectRef(trueobj);
	}
	else
		throw new NodeException(this, "Invalid output_id.", __FILE__, __LINE__);
}
};

}//namespace FD
