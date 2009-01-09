// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#include "BufferedNode.h"
#include "Vector.h"

using namespace std;

namespace FD {

class HardDetector;

DECLARE_NODE(HardDetector)
/*Node
 *
 * @name HardDetector
 * @category Communications:Receiver
 * @description Compute estimated bits from binary LLRs
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description LLR input sequence.
 *
 * @output_name OUTPUT
 * @output_type Vector<int>
 * @output_description Estimated bits.
 *
END*/


class HardDetector : public BufferedNode {
   
	int inputID;
	int outputID;
	
public:
HardDetector(string nodeName, ParameterSet params)
: BufferedNode(nodeName, params)
{
	inputID = addInput("INPUT");
	outputID = addOutput("OUTPUT");
}


void calculate(int output_id, int count, Buffer &out)
{
	int i;
	
	ObjectRef inputRef = getInput(inputID, count);
	const Vector<float> &inputVec = object_cast<Vector<float> >(inputRef);
	int length = inputVec.size();

	Vector<int> &output = *Vector<int>::alloc(length);

	out[count] = &output;

	for (i = 0; i < length; i++)
	{
		output[i] = (inputVec[i] >= 0.) ? 1 : 0;
	}

}
};

}//namespace FD
