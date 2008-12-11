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

class Histogram;

DECLARE_NODE(Histogram)
/*Node
 *
 * @name Histogram
 * @category Communications:Statistics
 * @description Generate a histogram of the input sequence. Bins are 
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input sequence.
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Generated histogram
 *
 * @parameter_name MINTHR
 * @parameter_type float
 * @parameter_description Minimum threshold
 * @parameter_value -1.
 *
 * @parameter_name MAXTHR
 * @parameter_type float
 * @parameter_description Maximum threshold
 * @parameter_value 1.
 *
 * @parameter_name NTHR
 * @parameter_type int
 * @parameter_description Number of thresholds
 * @parameter_value 21
END*/


class Histogram : public BufferedNode {
   
	int inputID;
	int outputID;
	float minthr, maxthr;
	int nthr;
	float step;

public:
Histogram(string nodeName, ParameterSet params)
: BufferedNode(nodeName, params)
{
	inputID = addInput("INPUT");
	outputID = addOutput("OUTPUT");

      if (parameters.exist("MINTHR"))
	 minthr = dereference_cast<float> (parameters.get("MINTHR"));
      else
	 minthr = -1.;

      if (parameters.exist("MAXTHR"))
	 maxthr = dereference_cast<float> (parameters.get("MAXTHR"));
      else
	 maxthr = 2. + minthr;

      if (parameters.exist("NTHR"))
	 nthr = dereference_cast<int> (parameters.get("NTHR"));
      else
	 nthr = 21;

	step = (maxthr - minthr) / float(nthr - 1.);
}



void calculate(int output_id, int count, Buffer &out)
{

	int i;
	
	ObjectRef inputRef = getInput(inputID, count);
	const Vector<float> &inputVec = object_cast<Vector<float> >(inputRef);
	int length = inputVec.size();

	Vector<float> &output = *Vector<float>::alloc(nthr + 2);
	out[count] = &output;

	for (i = 0; i < nthr + 2; i++)
	{
		output[i] = 0.;
	}

	for (i = 0; i < length; i++)
	{
		output[floor((MIN(MAX(inputVec[i], minthr - step), maxthr) - minthr + step) / step)]++;
	}

	for (i = 0; i < nthr + 2; i++)
	{
		output[i] /= double(length);
	}


}
};

}//namespace FD
