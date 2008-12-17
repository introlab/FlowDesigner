// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#include "Node.h"
#include "Vector.h"

using namespace std;

namespace FD {

class BitErrors;

DECLARE_NODE(BitErrors)
/*Node
 *
 * @name BitErrors
 * @category Communications:Statistics
 * @description Estimate the bit error rate (BER)
 *
 * @input_name INPUT
 * @input_type Vector<int>
 * @input_description Binary input sequence under test.
 *
 * @input_name INPUT_REF
 * @input_type Vector<int>
 * @input_description Reference binary input sequence.
 *
 * @output_name BER
 * @output_type float
 * @output_description Estimated bit error rate.
 *
 * @output_name NBERS
 * @output_type int
 * @output_description Number of bit errors.
 *
 * @output_name NBITS
 * @output_type int
 * @output_description Number of transmitted bits.
 * 
 * @output_name NOT_FINISHED
 * @output_type bool
 * @output_description true if NBERS >= MIN_NBERS.
 *
 * @parameter_name MIN_NBERS
 * @parameter_type int
 * @parameter_description Number of bit errors to count.
 * @parameter_value 100
 *
END*/


class BitErrors : public Node {
   
	int inputID, inputIDref;
	int outputIDber, outputIDnbits, outputIDnbers, outputIDcond;
	
	unsigned long int nbits;
	unsigned long int nbers;
	int min_nbers;

	int oflag;

	ObjectRef trueobj, falseobj;

public:
BitErrors(string nodeName, ParameterSet params)
: Node(nodeName, params)
{
	inputID = addInput("INPUT");
	inputIDref = addInput("INPUT_REF");
	outputIDber = addOutput("BER");
	outputIDnbits = addOutput("NBITS");
	outputIDnbers = addOutput("NBERS");
	outputIDcond = addOutput("NOT_FINISHED");

	if (parameters.exist("MIN_NBERS"))
		min_nbers = dereference_cast<int> (parameters.get("MIN_NBERS"));
	else min_nbers = 100;


	nbits = nbers = 0;

	oflag = 15;

	trueobj = ObjectRef(new Bool(true));
	falseobj = ObjectRef(new Bool(false));
}



ObjectRef getOutput(int output_id, int count)
{
	unsigned int i;
	unsigned int length;
	
	ObjectRef inputRef = getInput(inputID, count);
	ObjectRef inputrefRef = getInput(inputIDref, count);
	const Vector<int> &inputVec = object_cast<Vector<int> >(inputRef);
	const Vector<int> &inputrefVec = object_cast<Vector<int> >(inputrefRef);

	length = inputVec.size();

	if(oflag >= 15 && nbers < min_nbers)		// All outputs have been read.
	{
		if(inputrefVec.size() != length)
		{
			throw new NodeException(this, "Reference symbol sequence and received symbol sequence have different lengths", __FILE__, __LINE__);
		}
	
		for (i = 0; i < length; i++)
		{
			if(inputVec[i] != inputrefVec[i])
			{
				nbers++;
			}
		}
		nbits += length;
		oflag = 0;
	}

	if(output_id == outputIDnbits){
		oflag |= 1;
		return ObjectRef(Int::alloc(nbits));
	}
	else if(output_id == outputIDnbers)
	{
		oflag |= 2;
		return ObjectRef(Int::alloc(nbers));
	}
	else if(output_id == outputIDber)
	{
		oflag |= 4;
		return ObjectRef(Float::alloc(float(nbers) / float(nbits)));
	}
	else if(output_id == outputIDcond)
	{
		oflag |= 8;
		if((nbers >= min_nbers) && ((oflag & 7) >= 7)) return falseobj;
		else return trueobj;
	}
	else
		throw new NodeException(this, "Invalid output_id.", __FILE__, __LINE__);
}
};

}//namespace FD
